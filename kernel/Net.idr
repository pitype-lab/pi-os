module Net

import Data.Bits
import Data.C.Array8
import Data.C.Array8.Utils
import Data.C.Extra
import Data.String.Extra
import Data.Linear.Token
import Control.Monad.Reader
import MMIO
import Uart
import VirtIO

-- Set mscratch via ecall (traps into M-mode, fast path in trap.s)
%foreign "C:make_syscall"
prim__make_syscall : Bits64 -> PrimIO ()

-- Read mscratch — only valid from M-mode (called from trap handler)
%foreign "C:read_mscratch"
prim__read_mscratch : PrimIO Bits64

------------------------------------------------------------------------
-- NetEnv and NetIO
------------------------------------------------------------------------

public export
record NetEnv where
  constructor MkNetEnv
  dev       : VirtIODevice
  statePage : CArray8 World 4096
  rxVq      : CArray8 World 4096
  rxBuffer  : CArray8 World 4096
  txVq      : CArray8 World 4096
  txBuffer  : CArray8 World 4096

public export
NetIO : Type -> Type
NetIO = ReaderT NetEnv IO

-- Get the physical address of a CArray8 (for VirtIO register programming)
arrayAddr : CArray8 World n -> Bits64
arrayAddr arr = anyPtrToBits64 (unsafeUnwrap arr)

-- Reconstruct a CArray8 from a physical address
addrToArray : Bits64 -> CArray8 World n
addrToArray addr = unsafeWrap (bits64ToAnyPtr addr)

------------------------------------------------------------------------
-- Our IP constant
------------------------------------------------------------------------

OUR_IP : Bits32
OUR_IP = 0x0A00020F   -- 10.0.2.15

------------------------------------------------------------------------
-- Protocol header records
------------------------------------------------------------------------

record IPv4Header where
  constructor MkIPv4
  totalLen : Bits16
  ident    : Bits16
  flags    : Bits16
  ttl      : Bits8
  protocol : Bits8
  srcIp    : Bits32
  dstIp    : Bits32

record TCPHeader where
  constructor MkTCP
  srcPort    : Bits16
  dstPort    : Bits16
  seqNum     : Bits32
  ackNum     : Bits32
  dataOffset : Bits8
  flags      : Bits8
  window     : Bits16
  urgent     : Bits16

record ARPPacket where
  constructor MkARP
  htype : Bits16
  ptype : Bits16
  hlen  : Bits8
  plen  : Bits8
  oper  : Bits16
  spa   : Bits32
  tpa   : Bits32

-- Parsed protocol headers

record ParsedEthernet where
  constructor MkParsedEthernet
  ethertype : Bits16

record ParsedARP where
  constructor MkParsedARP
  oper : Bits16
  spa  : Bits32
  tpa  : Bits32

record ParsedIPv4 where
  constructor MkParsedIPv4
  ihl      : Nat
  totalLen : Bits16
  protocol : Bits8
  srcIp    : Bits32
  dstIp    : Bits32

record ParsedTCP where
  constructor MkParsedTCP
  srcPort    : Bits16
  dstPort    : Bits16
  seqNum     : Bits32
  dataOffset : Nat
  flags      : Bits8

------------------------------------------------------------------------
-- Protocol serializers (write to txBuffer at given offset)
------------------------------------------------------------------------

-- Write Ethernet header: dst MAC from (macSrc, macSrcOff), src MAC from (macSrc2, macSrcOff2)
writeEthernetHeader : CArray8 World n -> (off : Nat)
                   -> (dstSrc : CArray8 World dm) -> (dstOff : Nat)
                   -> (srcSrc : CArray8 World sm) -> (srcOff : Nat)
                   -> Bits16 -> IO ()
writeEthernetHeader buf off dstArr dstOff srcArr srcOff ethertype = do
  copyBytesAt buf off     dstArr dstOff 6
  copyBytesAt buf (off+6) srcArr srcOff 6
  writeBE16At buf (off+12) ethertype

writeARPPacket : CArray8 World n -> (off : Nat) -> ARPPacket
             -> (shaSrc : CArray8 World a) -> (shaOff : Nat)
             -> (thaSrc : CArray8 World b) -> (thaOff : Nat) -> IO ()
writeARPPacket buf off pkt shaSrc shaOff thaSrc thaOff = do
  writeBE16At buf off       pkt.htype
  writeBE16At buf (off+2)   pkt.ptype
  writeByteAt buf (off+4)   pkt.hlen
  writeByteAt buf (off+5)   pkt.plen
  writeBE16At buf (off+6)   pkt.oper
  copyBytesAt  buf (off+8)   shaSrc shaOff 6
  writeBE32At buf (off+14)  pkt.spa
  copyBytesAt  buf (off+18)  thaSrc thaOff 6
  writeBE32At buf (off+24)  pkt.tpa

writeIPv4Header : CArray8 World n -> (off : Nat) -> IPv4Header -> IO ()
writeIPv4Header buf off hdr = do
  writeByteAt buf off       0x45
  writeByteAt buf (off+1)   0x00
  writeBE16At buf (off+2)   hdr.totalLen
  writeBE16At buf (off+4)   hdr.ident
  writeBE16At buf (off+6)   hdr.flags
  writeByteAt buf (off+8)   hdr.ttl
  writeByteAt buf (off+9)   hdr.protocol
  writeBE16At buf (off+10)  0     -- checksum placeholder
  writeBE32At buf (off+12)  hdr.srcIp
  writeBE32At buf (off+16)  hdr.dstIp

writeTCPHeader : CArray8 World n -> (off : Nat) -> TCPHeader -> IO ()
writeTCPHeader buf off hdr = do
  writeBE16At buf off       hdr.srcPort
  writeBE16At buf (off+2)   hdr.dstPort
  writeBE32At buf (off+4)   hdr.seqNum
  writeBE32At buf (off+8)   hdr.ackNum
  writeByteAt buf (off+12)  hdr.dataOffset
  writeByteAt buf (off+13)  hdr.flags
  writeBE16At buf (off+14)  hdr.window
  writeBE16At buf (off+16)  0     -- checksum placeholder
  writeBE16At buf (off+18)  hdr.urgent

------------------------------------------------------------------------
-- Protocol parsers (read from rxBuffer at given offset)
------------------------------------------------------------------------

parseEthernetHeader : CArray8 World n -> (off : Nat) -> IO ParsedEthernet
parseEthernetHeader buf off = do
  et <- readBE16At buf (off+12)
  pure $ MkParsedEthernet et

parseARPPacket : CArray8 World n -> (off : Nat) -> IO ParsedARP
parseARPPacket buf off = do
  op  <- readBE16At buf (off+6)
  spa <- readBE32At buf (off+14)
  tp  <- readBE32At buf (off+24)
  pure $ MkParsedARP op spa tp

parseIPv4Header : CArray8 World n -> (off : Nat) -> IO ParsedIPv4
parseIPv4Header buf off = do
  ihlByte  <- readByteAt buf off
  let ihl : Nat = cast ((ihlByte .&. 0x0F) * 4)
  totalLen <- readBE16At buf (off+2)
  proto    <- readByteAt buf (off+9)
  srcIp    <- readBE32At buf (off+12)
  dstIp    <- readBE32At buf (off+16)
  pure $ MkParsedIPv4 ihl totalLen proto srcIp dstIp

parseTCPHeader : CArray8 World n -> (off : Nat) -> IO ParsedTCP
parseTCPHeader buf off = do
  sp <- readBE16At buf off
  dp <- readBE16At buf (off+2)
  sn <- readBE32At buf (off+4)
  doByte <- readByteAt buf (off+12)
  let dataOff : Nat = cast ((doByte `shiftR` 4) * 4)
  fl <- readByteAt buf (off+13)
  pure $ MkParsedTCP sp dp sn dataOff fl

------------------------------------------------------------------------
-- Checksum helpers
------------------------------------------------------------------------

patchIPChecksum : CArray8 World n -> (ipOff : Nat) -> IO ()
patchIPChecksum buf ipOff = do
  acc <- onesCompSumAt buf ipOff 20
  writeBE16At buf (ipOff + 10) (foldChecksum acc)

patchTCPChecksum : CArray8 World n -> (scratchOff : Nat)
                -> Bits32 -> Bits32 -> Bits16 -> (tcpOff : Nat) -> IO ()
patchTCPChecksum buf scratchOff srcIp dstIp tcpLen tcpOff = do
  -- Write pseudo-header at scratchOff
  writeBE32At buf scratchOff         srcIp
  writeBE32At buf (scratchOff + 4)   dstIp
  writeByteAt buf (scratchOff + 8)   0
  writeByteAt buf (scratchOff + 9)   6      -- protocol = TCP
  writeBE16At buf (scratchOff + 10)  tcpLen
  acc1 <- onesCompSumAt buf scratchOff 12
  acc2 <- onesCompSumAt buf tcpOff (cast tcpLen)
  writeBE16At buf (tcpOff + 16) (foldChecksum (acc1 + acc2))

------------------------------------------------------------------------
-- TX send helper
------------------------------------------------------------------------

sendTx : Nat -> NetIO ()
sendTx pktLen = do
  env <- ask
  let st = env.statePage
      txVqArr = env.txVq
  txDescRaw  <- readNetField st TxDescAddr
  txAvailRaw <- readNetField st TxAvailAddr
  txIdx      <- readNetField16 st TxAvailIdx

  let txDescA  : CArray8 World 4096 = addrToArray txDescRaw
      txAvailA : CArray8 World 4096 = addrToArray txAvailRaw

  -- Update descriptor length (offset 8 into descriptor)
  writeLE32At txDescA 8 (cast pktLen)

  -- avail ring[0] = descriptor 0
  writeLE16At txAvailA 4 0

  -- Bump avail idx
  let newIdx = txIdx + 1
  writeLE16At txAvailA 2 newIdx
  writeNetField16 st TxAvailIdx newIdx

  -- Notify queue 1
  writeVirtIO env.dev QueueNotify 1

------------------------------------------------------------------------
-- ARP handling
------------------------------------------------------------------------

sendArpReply : NetIO ()
sendArpReply = do
  env <- ask
  let st = env.statePage
      tx = env.txBuffer
  peerIp <- readNetField32 st PeerIp

  -- Zero out VirtIO net header (10 bytes)
  zeroBytesAt tx 0 10

  let eth = 10
  liftIO $ writeEthernetHeader tx eth st (netMacOffset PeerMac) st (netMacOffset OurMac) 0x0806

  let arp = eth + 14
  liftIO $ writeARPPacket tx arp
    (MkARP { htype = 0x0001, ptype = 0x0800, hlen = 6, plen = 4
           , oper = 0x0002, spa = OUR_IP, tpa = peerIp })
    st (netMacOffset OurMac)    -- sha = our MAC
    st (netMacOffset PeerMac)   -- tha = peer MAC

  sendTx 52   -- 10 + 14 + 28

handleArp : Nat -> NetIO ()
handleArp frameOff = do
  env <- ask
  let rx = env.rxBuffer
      st = env.statePage
  arp <- liftIO $ parseARPPacket rx (frameOff + 14)
  println $ "Net: ARP oper=" ++ b64ToHexString (cast arp.oper) ++ " tpa=0x" ++ b64ToHexString (cast arp.tpa)
  when (arp.oper == 1 && arp.tpa == OUR_IP) $ do
    println "Net: ARP request for us, sending reply"
    -- Save sender IP (stored as LE in state page)
    writeNetField32 st PeerIp arp.spa
    -- Save peer MAC from Ethernet src (offset 6 in frame)
    copyBytesAt st (netMacOffset PeerMac) rx (frameOff + 6) 6
    sendArpReply

------------------------------------------------------------------------
-- SYN-ACK construction
------------------------------------------------------------------------

sendSynAck : NetIO ()
sendSynAck = do
  env <- ask
  let st = env.statePage
      tx = env.txBuffer
  peerIp   <- readNetField32 st PeerIp
  peerPort <- readNetField16 st PeerPort
  peerSeqN <- readNetField32 st PeerSeqNext
  ourSeq   <- readNetField32 st OurSeq

  zeroBytesAt tx 0 10

  let eth = 10
      ipOff = eth + 14
      tcpOff = ipOff + 20

  liftIO $ writeEthernetHeader tx eth st (netMacOffset PeerMac) st (netMacOffset OurMac) 0x0800

  liftIO $ writeIPv4Header tx ipOff $ MkIPv4
    { totalLen = 40, ident = 0, flags = 0, ttl = 64, protocol = 6
    , srcIp = OUR_IP, dstIp = peerIp }

  liftIO $ writeTCPHeader tx tcpOff $ MkTCP
    { srcPort = 80, dstPort = peerPort, seqNum = ourSeq, ackNum = peerSeqN
    , dataOffset = 0x50, flags = 0x12, window = 65535, urgent = 0 }

  liftIO $ patchIPChecksum tx ipOff
  liftIO $ patchTCPChecksum tx 2048 OUR_IP peerIp 20 tcpOff

  writeNetField32 st OurSeq (ourSeq + 1)
  sendTx 64

------------------------------------------------------------------------
-- HTTP response
------------------------------------------------------------------------

sendHttpResponse : Nat -> NetIO ()
sendHttpResponse payloadLen = do
  env <- ask
  let st = env.statePage
      tx = env.txBuffer
  peerIp   <- readNetField32 st PeerIp
  peerPort <- readNetField16 st PeerPort
  peerSeqN <- readNetField32 st PeerSeqNext
  ourSeq   <- readNetField32 st OurSeq

  zeroBytesAt tx 0 10

  let eth = 10
      ipOff = eth + 14
      tcpOff = ipOff + 20
      payloadOff = tcpOff + 20

  let httpResp : List Bits8 =
        -- HTTP/1.1 200 OK\r\n
        [ 0x48, 0x54, 0x54, 0x50, 0x2F, 0x31, 0x2E, 0x31
        , 0x20, 0x32, 0x30, 0x30, 0x20, 0x4F, 0x4B, 0x0D, 0x0A
        -- Content-Length: 11\r\n
        , 0x43, 0x6F, 0x6E, 0x74, 0x65, 0x6E, 0x74, 0x2D
        , 0x4C, 0x65, 0x6E, 0x67, 0x74, 0x68, 0x3A, 0x20
        , 0x31, 0x31, 0x0D, 0x0A
        -- Connection: close\r\n
        , 0x43, 0x6F, 0x6E, 0x6E, 0x65, 0x63, 0x74, 0x69
        , 0x6F, 0x6E, 0x3A, 0x20, 0x63, 0x6C, 0x6F, 0x73
        , 0x65, 0x0D, 0x0A
        -- \r\n
        , 0x0D, 0x0A
        -- Hello World
        , 0x48, 0x65, 0x6C, 0x6C, 0x6F, 0x20, 0x57, 0x6F
        , 0x72, 0x6C, 0x64
        ]
      httpLen : Nat = 69

  writeListAt tx payloadOff httpResp

  let tcpLen : Bits16 = cast (20 + httpLen)
      ipTotal : Bits16 = cast (20 + 20 + httpLen)
      newPeerSeq = peerSeqN + cast payloadLen

  liftIO $ writeEthernetHeader tx eth st (netMacOffset PeerMac) st (netMacOffset OurMac) 0x0800

  liftIO $ writeIPv4Header tx ipOff $ MkIPv4
    { totalLen = ipTotal, ident = 0, flags = 0, ttl = 64, protocol = 6
    , srcIp = OUR_IP, dstIp = peerIp }

  liftIO $ writeTCPHeader tx tcpOff $ MkTCP
    { srcPort = 80, dstPort = peerPort, seqNum = ourSeq, ackNum = newPeerSeq
    , dataOffset = 0x50, flags = 0x19, window = 65535, urgent = 0 }

  liftIO $ patchIPChecksum tx ipOff
  liftIO $ patchTCPChecksum tx 2048 OUR_IP peerIp tcpLen tcpOff

  writeNetField32 st PeerSeqNext newPeerSeq
  writeNetField32 st OurSeq (ourSeq + cast httpLen)

  sendTx (10 + 14 + 20 + 20 + httpLen)

------------------------------------------------------------------------
-- HTTP payload printing
------------------------------------------------------------------------

printPayload : Nat -> Nat -> NetIO ()
printPayload off len = do
  env <- ask
  let rx = env.rxBuffer
  println "=== HTTP REQUEST ==="
  let go : CArray8 World 4096 -> Nat -> IO ()
      go buf i =
        if i >= len
          then pure ()
          else do
            c <- readByteAt buf (off + i)
            if (c == 0x0A || c == 0x0D || (c >= 0x20 && c < 0x7F))
              then write_mmio_bits8 UART c
              else write_mmio_bits8 UART (cast '.')
            go buf (S i)
  liftIO $ go rx 0
  println "\n==="

------------------------------------------------------------------------
-- TCP handling
------------------------------------------------------------------------

handleTcp : Nat -> Nat -> NetIO ()
handleTcp ipOff tcpOff = do
  env <- ask
  let rx = env.rxBuffer
      st = env.statePage
  tcp <- liftIO $ parseTCPHeader rx tcpOff
  println $ "Net: TCP dstPort=" ++ b64ToHexString (cast tcp.dstPort) ++ " flags=0x" ++ b64ToHexString (cast tcp.flags)
  when (tcp.dstPort == 80) $ do
    let isSyn = tcp.flags .&. 0x02
        isAck = tcp.flags .&. 0x10
        isPsh = tcp.flags .&. 0x08
    if (isSyn /= 0 && isAck == 0)
      then do
        println "Net: TCP SYN received, sending SYN-ACK"
        writeNetField16 st PeerPort tcp.srcPort
        -- Read peer IP from IP header src field (offset ipOff+12), store as LE
        peerIp <- liftIO $ readBE32At rx (ipOff + 12)
        writeNetField32 st PeerIp peerIp
        writeNetField32 st PeerSeqNext (tcp.seqNum + 1)
        sendSynAck
        println "Net: SYN-ACK sent"
      else if isPsh /= 0
        then do
          println "Net: TCP PSH received, printing payload"
          let payloadStart = tcpOff + tcp.dataOffset
          ip <- liftIO $ parseIPv4Header rx ipOff
          let tcpSegLen : Nat = cast ip.totalLen `minus` ip.ihl
              payloadLen = tcpSegLen `minus` tcp.dataOffset
          println $ "Net: payloadLen=" ++ b64ToHexString (cast payloadLen)
          when (payloadLen > 0) $ do
            printPayload payloadStart payloadLen
            sendHttpResponse payloadLen
            println "Net: HTTP response sent"
        else println $ "Net: TCP no-op flags=0x" ++ b64ToHexString (cast tcp.flags)

------------------------------------------------------------------------
-- IPv4 handling
------------------------------------------------------------------------

handleIPv4 : Nat -> Nat -> NetIO ()
handleIPv4 frameOff frameLen = do
  env <- ask
  let rx = env.rxBuffer
      ipOff = frameOff + 14
  ip <- liftIO $ parseIPv4Header rx ipOff
  println $ "Net: IPv4 proto=" ++ b64ToHexString (cast ip.protocol) ++ " dstIp=0x" ++ b64ToHexString (cast ip.dstIp)
  when (ip.protocol == 6 && ip.dstIp == OUR_IP) $ do
    let tcpOff = ipOff + ip.ihl
    handleTcp ipOff tcpOff

------------------------------------------------------------------------
-- Frame dispatch
------------------------------------------------------------------------

dispatchFrame : Nat -> Nat -> NetIO ()
dispatchFrame frameOff frameLen = do
  env <- ask
  let rx = env.rxBuffer
  eth <- liftIO $ parseEthernetHeader rx frameOff
  println $ "Net: ethertype=0x" ++ b64ToHexString (cast eth.ethertype) ++ " frameLen=" ++ b64ToHexString (cast frameLen)
  case eth.ethertype of
    0x0806 => handleArp frameOff
    0x0800 => handleIPv4 frameOff frameLen
    _      => println $ "Net: unknown ethertype, ignoring"

------------------------------------------------------------------------
-- IRQ handler body (runs in NetIO)
------------------------------------------------------------------------

handleIrqBody : NetIO ()
handleIrqBody = do
  env <- ask
  let st = env.statePage
      rxVqArr = env.rxVq

  -- Read avail/used ring addresses from state page
  availRaw <- readNetField st RxAvailAddr
  rxUsedRaw <- readNetField st RxUsedAddr
  idx <- readNetField16 st RxAvailIdx

  let availA : CArray8 World 4096 = addrToArray availRaw
      rxUsedA : CArray8 World 4096 = addrToArray rxUsedRaw

  -- 1. Ack the virtio interrupt
  isr    <- readVirtIO env.dev InterruptStatus
  status <- readVirtIO env.dev Status
  println $ "Net: IRQ isr=0x" ++ b64ToHexString (cast isr) ++ " status=0x" ++ b64ToHexString (cast status)
  writeVirtIO env.dev InterruptACK isr

  -- 2. Check for used descriptor
  usedIdx  <- readLE16At rxUsedA 2
  lastUsed <- readNetField16 st LastUsedIdx
  println $ "Net: idx=" ++ b64ToHexString (cast idx) ++ " usedIdx=" ++ b64ToHexString (cast usedIdx) ++ " lastUsed=" ++ b64ToHexString (cast lastUsed) ++ " rxUsed=0x" ++ b64ToHexString rxUsedRaw
  if usedIdx > lastUsed
    then do
      pktLen32 <- readLE32At rxUsedA 8
      println $ "Net: pktLen=" ++ b64ToHexString (cast pktLen32)
      when (pktLen32 > 10) $ do
        let frameOff = 10   -- skip VirtIO net header
            frameLen : Nat = cast pktLen32 `minus` 10
        println $ "Net: frameLen=" ++ b64ToHexString (cast frameLen)
        when (frameLen > 14) $
          dispatchFrame frameOff frameLen

      -- Update lastUsed
      writeNetField16 st LastUsedIdx usedIdx

      -- Re-arm: put descriptor 0 back in available ring
      let newAvailIdx = idx + 1
      writeLE16At availA 4 0           -- ring[0] = descriptor 0
      writeLE16At availA 2 newAvailIdx -- bump avail.idx
      writeNetField16 st RxAvailIdx newAvailIdx
      writeVirtIO env.dev QueueNotify 0

      println $ "Net: processed usedIdx=" ++ b64ToHexString (cast usedIdx)
    else
      println "Net: no new used descriptor"

------------------------------------------------------------------------
-- IRQ handler (entry point from trap handler)
------------------------------------------------------------------------

export
handleNetIrq : IO ()
handleNetIrq = do
  stateRaw <- primIO prim__read_mscratch
  if stateRaw == 0
    then pure ()
    else do
      let stPage : CArray8 World 4096 = addrToArray stateRaw
      baseRaw <- readNetField stPage VirtioBase
      case mkVirtIODevice baseRaw of
        Nothing  => println "Net: IRQ virtio base out of range"
        Just dev => do
          rxVqRaw  <- readNetField stPage RxVqAddr
          rxBufRaw <- readNetField stPage RxBufferAddr
          txVqRaw  <- readNetField stPage TxVqAddr
          txBufRaw <- readNetField stPage TxBufferAddr
          let env = MkNetEnv
                { dev       = dev
                , statePage = stPage
                , rxVq      = addrToArray rxVqRaw
                , rxBuffer  = addrToArray rxBufRaw
                , txVq      = addrToArray txVqRaw
                , txBuffer  = addrToArray txBufRaw
                }
          runReaderT env handleIrqBody

------------------------------------------------------------------------
-- Network setup
------------------------------------------------------------------------
export
setupNetwork : VirtIODevice -> IO ()
setupNetwork dev = do
  -- Allocate 5 zeroed arrays via the urefc C heap
  stPage   <- runIO $ calloc1 4096
  rxVqArr  <- runIO $ calloc1 4096
  rxBufArr <- runIO $ calloc1 4096
  txVqArr  <- runIO $ calloc1 4096
  txBufArr <- runIO $ calloc1 4096

  let env = MkNetEnv dev stPage rxVqArr rxBufArr txVqArr txBufArr

  runReaderT env (the (NetIO ()) $ do
    println "Setup network"

    -- RESET
    writeVirtIO dev Status 0
    -- ACK
    writeVirtIO dev Status 1
    -- DRIVER
    writeVirtIO dev Status 3

    -- Read device features
    features <- readVirtIO dev DeviceFeatures
    println $ "Net: device features=0x" ++ b64ToHexString (cast features)

    -- Accept features but clear MRG_RXBUF (bit 15) so header stays 10 bytes
    let drvFeatures = features .&. complement (1 `shiftL` 15)
    writeVirtIO dev DriverFeatures drvFeatures
    -- FEATURES_OK
    writeVirtIO dev Status 11

    -- ---- RX Queue (queue 0) ----
    writeVirtIO dev QueueSel 0
    writeVirtIO dev QueueSize 4096
    writeVirtIO dev QueueNum 1
    writeVirtIO dev QueueAlign 4

    -- RX Descriptor 0: addr=rxBuffer, len=2048, flags=WRITE(2), next=0
    writeLE64At rxVqArr 0 (arrayAddr rxBufArr)    -- descriptor addr
    writeLE32At rxVqArr 8 2048                     -- descriptor len
    writeLE16At rxVqArr 12 2                       -- flags = WRITE
    writeLE16At rxVqArr 14 0                       -- next
    println $ "Net: RX desc at 0x" ++ b64ToHexString (arrayAddr rxVqArr) ++ " buffer=0x" ++ b64ToHexString (arrayAddr rxBufArr)

    -- RX Available ring (starts at offset 16 in rxVq): flags=0, idx=1, ring[0]=0
    writeLE16At rxVqArr 16 0
    writeLE16At rxVqArr 18 1
    writeLE16At rxVqArr 20 0
    println "Net: RX avail ring idx=1 ring[0]=0"

    -- RX Used ring (at offset 24): flags=0, idx=0
    writeLE16At rxVqArr 24 0
    writeLE16At rxVqArr 26 0

    -- QueuePFN for RX
    writeVirtIO dev QueuePFN (cast $ shiftR (arrayAddr rxVqArr) 12)

    -- ---- TX Queue (queue 1) ----
    writeVirtIO dev QueueSel 1
    writeVirtIO dev QueueNum 1
    writeVirtIO dev QueueAlign 4

    -- TX Descriptor 0: addr=txBuffer, len=2048, flags=0, next=0
    writeLE64At txVqArr 0 (arrayAddr txBufArr)
    writeLE32At txVqArr 8 2048
    writeLE16At txVqArr 12 0
    writeLE16At txVqArr 14 0

    -- TX Available ring: flags=0, idx=0, ring[0]=0
    writeLE16At txVqArr 16 0
    writeLE16At txVqArr 18 0
    writeLE16At txVqArr 20 0

    -- QueuePFN for TX
    writeVirtIO dev QueuePFN (cast $ shiftR (arrayAddr txVqArr) 12)

    -- DRIVER_OK
    writeVirtIO dev Status 7

    -- Notify device that RX descriptor is available
    writeVirtIO dev QueueNotify 0
    println "Net: notified device of RX descriptor"

    -- Read MAC from VirtIO config space
    (mac0, mac1, mac2, mac3, mac4, mac5) <- readVirtIOMAC dev
    println $ "Net: MAC=" ++ b64ToHexString (cast mac0) ++ ":" ++ b64ToHexString (cast mac1) ++ ":" ++ b64ToHexString (cast mac2) ++ ":" ++ b64ToHexString (cast mac3) ++ ":" ++ b64ToHexString (cast mac4) ++ ":" ++ b64ToHexString (cast mac5)

    -- Write state page fields
    writeNetField stPage VirtioBase (getVirtIOAddr dev)
    writeNetField stPage RxAvailAddr (arrayAddr rxVqArr + 16)  -- avail ring is at rxVq+16
    writeNetField16 stPage RxAvailIdx 1
    writeNetField16 stPage LastUsedIdx 0
    writeNetField stPage RxUsedAddr (arrayAddr rxVqArr + 24)   -- used ring is at rxVq+24
    writeNetField stPage RxBufferAddr (arrayAddr rxBufArr)
    writeNetField stPage TxDescAddr (arrayAddr txVqArr)        -- TX descriptor at txVq base
    writeNetField stPage TxAvailAddr (arrayAddr txVqArr + 16)  -- TX avail at txVq+16
    writeNetField stPage TxBufferAddr (arrayAddr txBufArr)
    writeNetField16 stPage TxAvailIdx 0
    writeNetField stPage RxVqAddr (arrayAddr rxVqArr)
    writeNetField stPage TxVqAddr (arrayAddr txVqArr)
    writeNetMAC stPage OurMac mac0 mac1 mac2 mac3 mac4 mac5
    writeNetField32 stPage OurSeq 0x1000
    writeNetField32 stPage PeerSeqNext 0
    writeNetField16 stPage PeerPort 0
    writeNetField32 stPage PeerIp 0

    -- Set mscratch to state page address via ecall into M-mode
    liftIO $ primIO $ prim__make_syscall (arrayAddr stPage)

    println "Virtio network setup complete."
    )

