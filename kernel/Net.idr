module Net

import Data.Bits
import Data.C.Array8
import Data.C.Extra
import Data.Nat
import Data.String.Extra
import Heap
import Kernel
import MMIO
import Pages
import Prelude.Extra.Num
import Uart
import VirtIO

-- Set mscratch via ecall (traps into M-mode, fast path in trap.s)
%foreign "C:make_syscall"
prim__make_syscall : Bits64 -> PrimIO ()

-- Read mscratch — only valid from M-mode (called from trap handler)
%foreign "C:read_mscratch"
prim__read_mscratch : PrimIO Bits64

------------------------------------------------------------------------
-- Byte-order helpers (operate on raw buffer addresses)
------------------------------------------------------------------------

readBE16 : Bits64 -> IO Bits16
readBE16 addr = do
  hi <- primIO $ prim__deref_bits8 addr
  lo <- primIO $ prim__deref_bits8 (addr + 1)
  pure $ (cast hi `shiftL` 8) .|. cast lo

readBE32 : Bits64 -> IO Bits32
readBE32 addr = do
  b0 <- primIO $ prim__deref_bits8 addr
  b1 <- primIO $ prim__deref_bits8 (addr + 1)
  b2 <- primIO $ prim__deref_bits8 (addr + 2)
  b3 <- primIO $ prim__deref_bits8 (addr + 3)
  pure $ (cast b0 `shiftL` 24) .|. (cast b1 `shiftL` 16) .|.
         (cast b2 `shiftL` 8)  .|. cast b3

writeBE16 : Bits64 -> Bits16 -> IO ()
writeBE16 addr val = do
  primIO $ prim__set_bits8 addr       (cast $ shiftR val 8)
  primIO $ prim__set_bits8 (addr + 1) (cast val)

writeBE32 : Bits64 -> Bits32 -> IO ()
writeBE32 addr val = do
  primIO $ prim__set_bits8 addr       (cast $ shiftR val 24)
  primIO $ prim__set_bits8 (addr + 1) (cast $ shiftR val 16)
  primIO $ prim__set_bits8 (addr + 2) (cast $ shiftR val 8)
  primIO $ prim__set_bits8 (addr + 3) (cast val)

writeByte : Bits64 -> Bits8 -> IO ()
writeByte addr val = primIO $ prim__set_bits8 addr val

readByte : Bits64 -> IO Bits8
readByte addr = primIO $ prim__deref_bits8 addr

------------------------------------------------------------------------
-- Utilities (operate on raw buffer addresses)
------------------------------------------------------------------------

copyMac : Bits64 -> Bits64 -> IO ()
copyMac dst src = do
  let go : Bits64 -> IO ()
      go 6 = pure ()
      go i = do
        b <- readByte (src + i)
        writeByte (dst + i) b
        go (i + 1)
  go 0

writeZeros : Bits64 -> Bits64 -> IO ()
writeZeros addr n = do
  let go : Bits64 -> IO ()
      go 0 = pure ()
      go i = do
        writeByte (addr + (n - i)) 0
        go (i - 1)
  go n

-- Write a list of bytes to a buffer address
writeByteList : Bits64 -> List Bits8 -> IO ()
writeByteList _ []        = pure ()
writeByteList addr (b :: bs) = do
  writeByte addr b
  writeByteList (addr + 1) bs

------------------------------------------------------------------------
-- Internet checksum (operates on raw buffer addresses)
------------------------------------------------------------------------

-- Accumulate one's-complement sum over n bytes at addr
onesCompSum : Bits64 -> Bits64 -> IO Bits32
onesCompSum addr n = go 0 0
  where
    go : Bits64 -> Bits32 -> IO Bits32
    go i acc =
      if i >= n
        then pure acc
        else if i + 1 < n
          then do
            hi <- readByte (addr + i)
            lo <- readByte (addr + i + 1)
            let w : Bits32 = (cast hi `shiftL` 8) .|. cast lo
            go (i + 2) (acc + w)
          else do
            hi <- readByte (addr + i)
            go (i + 1) (acc + (cast hi `shiftL` 8))

foldChecksum : Bits32 -> Bits16
foldChecksum acc =
  let folded = (acc `shiftR` 16) + (acc .&. 0xFFFF)
      folded2 = folded + (folded `shiftR` 16)
  in complement (cast folded2)

ipChecksum : Bits64 -> Bits64 -> IO Bits16
ipChecksum addr len = do
  acc <- onesCompSum addr len
  pure (foldChecksum acc)

------------------------------------------------------------------------
-- Protocol header records
------------------------------------------------------------------------

-- MAC addresses are represented as the raw address where the 6 bytes live.
-- Serializers use copyMac to write them into packet buffers.
-- This avoids allocating intermediate structures.
MacAddr : Type
MacAddr = Bits64

record EthernetHeader where
  constructor MkEthernet
  dstMac    : MacAddr
  srcMac    : MacAddr
  ethertype : Bits16

record ARPPacket where
  constructor MkARP
  htype : Bits16    -- hardware type (0x0001 = Ethernet)
  ptype : Bits16    -- protocol type (0x0800 = IPv4)
  hlen  : Bits8     -- hardware addr length (6)
  plen  : Bits8     -- protocol addr length (4)
  oper  : Bits16    -- operation (1=request, 2=reply)
  sha   : MacAddr   -- sender hardware address
  spa   : Bits32    -- sender protocol address
  tha   : MacAddr   -- target hardware address
  tpa   : Bits32    -- target protocol address

record IPv4Header where
  constructor MkIPv4
  totalLen : Bits16
  ident    : Bits16
  flags    : Bits16   -- flags + fragment offset
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
  dataOffset : Bits8    -- upper nibble = data offset in 32-bit words
  flags      : Bits8
  window     : Bits16
  urgent     : Bits16

-- Parsed versions (returned from parsers, pure data)

record ParsedEthernet where
  constructor MkParsedEthernet
  ethertype : Bits16
  srcMacOff : Bits64    -- offset in frame where src MAC starts (for copyMac)

record ParsedARP where
  constructor MkParsedARP
  oper     : Bits16
  sha      : Bits64    -- offset to sender hardware address (for copyMac)
  spa      : Bits32    -- sender protocol address
  tpa      : Bits32    -- target protocol address

record ParsedIPv4 where
  constructor MkParsedIPv4
  ihl      : Bits64    -- header length in bytes
  totalLen : Bits16
  protocol : Bits8
  srcIp    : Bits32
  dstIp    : Bits32

record ParsedTCP where
  constructor MkParsedTCP
  srcPort    : Bits16
  dstPort    : Bits16
  seqNum     : Bits32
  dataOffset : Bits64   -- header length in bytes (derived from data offset nibble)
  flags      : Bits8

------------------------------------------------------------------------
-- Protocol serializers
------------------------------------------------------------------------

-- Write an Ethernet header (14 bytes) at the given address.
-- Returns the address immediately after the header.
writeEthernetHeader : Bits64 -> EthernetHeader -> IO Bits64
writeEthernetHeader addr hdr = do
  copyMac addr       hdr.dstMac
  copyMac (addr + 6) hdr.srcMac
  writeBE16 (addr + 12) hdr.ethertype
  pure (addr + 14)

-- Write an ARP packet (28 bytes) at the given address.
writeARPPacket : Bits64 -> ARPPacket -> IO ()
writeARPPacket addr pkt = do
  writeBE16 addr       pkt.htype
  writeBE16 (addr + 2) pkt.ptype
  writeByte (addr + 4) pkt.hlen
  writeByte (addr + 5) pkt.plen
  writeBE16 (addr + 6) pkt.oper
  copyMac (addr + 8)   pkt.sha
  writeBE32 (addr + 14) pkt.spa
  copyMac (addr + 18)  pkt.tha
  writeBE32 (addr + 24) pkt.tpa

-- Write an IPv4 header (20 bytes, IHL=5) at the given address.
-- Checksum field is set to 0; caller must compute and patch it after.
-- Returns the address immediately after the header.
writeIPv4Header : Bits64 -> IPv4Header -> IO Bits64
writeIPv4Header addr hdr = do
  writeByte (addr)     0x45           -- version=4, IHL=5
  writeByte (addr + 1) 0x00           -- DSCP/ECN
  writeBE16 (addr + 2) hdr.totalLen
  writeBE16 (addr + 4) hdr.ident
  writeBE16 (addr + 6) hdr.flags
  writeByte (addr + 8) hdr.ttl
  writeByte (addr + 9) hdr.protocol
  writeBE16 (addr + 10) 0             -- checksum placeholder
  writeBE32 (addr + 12) hdr.srcIp
  writeBE32 (addr + 16) hdr.dstIp
  pure (addr + 20)

-- Write a TCP header (20 bytes, data offset=5) at the given address.
-- Checksum field is set to 0; caller must compute and patch it after.
-- Returns the address immediately after the header.
writeTCPHeader : Bits64 -> TCPHeader -> IO Bits64
writeTCPHeader addr hdr = do
  writeBE16 (addr)      hdr.srcPort
  writeBE16 (addr + 2)  hdr.dstPort
  writeBE32 (addr + 4)  hdr.seqNum
  writeBE32 (addr + 8)  hdr.ackNum
  writeByte (addr + 12) hdr.dataOffset
  writeByte (addr + 13) hdr.flags
  writeBE16 (addr + 14) hdr.window
  writeBE16 (addr + 16) 0             -- checksum placeholder
  writeBE16 (addr + 18) hdr.urgent
  pure (addr + 20)

-- Patch IP checksum in-place (writes at offset 10 from IP header start)
patchIPChecksum : Bits64 -> IO ()
patchIPChecksum ipAddr = do
  csum <- ipChecksum ipAddr 20
  writeBE16 (ipAddr + 10) csum

-- Write TCP pseudo-header to scratch, compute checksum over pseudo-header + TCP segment.
-- Patches TCP checksum in-place (at offset 16 from TCP header start).
patchTCPChecksum : Bits64 -> Bits32 -> Bits32 -> Bits16 -> Bits64 -> IO ()
patchTCPChecksum scratch srcIp dstIp tcpLen tcpAddr = do
  writeBE32 scratch         srcIp
  writeBE32 (scratch + 4)   dstIp
  writeByte (scratch + 8)   0
  writeByte (scratch + 9)   6      -- protocol = TCP
  writeBE16 (scratch + 10)  tcpLen
  acc1 <- onesCompSum scratch 12
  acc2 <- onesCompSum tcpAddr (cast tcpLen)
  writeBE16 (tcpAddr + 16) (foldChecksum (acc1 + acc2))

------------------------------------------------------------------------
-- Protocol parsers
------------------------------------------------------------------------

parseEthernetHeader : Bits64 -> IO ParsedEthernet
parseEthernetHeader addr = do
  et <- readBE16 (addr + 12)
  pure $ MkParsedEthernet et (addr + 6)

parseARPPacket : Bits64 -> IO ParsedARP
parseARPPacket addr = do
  op  <- readBE16 (addr + 6)
  spa <- readBE32 (addr + 14)
  tp  <- readBE32 (addr + 24)
  pure $ MkParsedARP op (addr + 8) spa tp

parseIPv4Header : Bits64 -> IO ParsedIPv4
parseIPv4Header addr = do
  ihlByte  <- readByte addr
  let ihl : Bits64 = cast ((ihlByte .&. 0x0F) * 4)
  totalLen <- readBE16 (addr + 2)
  proto    <- readByte (addr + 9)
  srcIp    <- readBE32 (addr + 12)
  dstIp    <- readBE32 (addr + 16)
  pure $ MkParsedIPv4 ihl totalLen proto srcIp dstIp

parseTCPHeader : Bits64 -> IO ParsedTCP
parseTCPHeader addr = do
  sp <- readBE16 addr
  dp <- readBE16 (addr + 2)
  sn <- readBE32 (addr + 4)
  doByte <- readByte (addr + 12)
  let dataOff : Bits64 = cast ((doByte `shiftR` 4) * 4)
  fl <- readByte (addr + 13)
  pure $ MkParsedTCP sp dp sn dataOff fl

------------------------------------------------------------------------
-- TX send helper
------------------------------------------------------------------------

sendTx : HeapAddr -> VirtIODevice -> Bits64 -> IO ()
sendTx st dev pktLen = do
  txDescRaw  <- readNetField st TxDescAddr
  txAvailRaw <- readNetField st TxAvailAddr
  txIdx      <- readNetField16 st TxAvailIdx
  case (mkHeapAddr txDescRaw, mkHeapAddr txAvailRaw) of
    (Just txDescH, Just txAvailH) => do
      -- Update descriptor length (offset 8 into descriptor)
      case (offsetHeapAddr txDescH 8, offsetHeapAddr txAvailH 2, offsetHeapAddr txAvailH 4) of
        (Just txDesc_8, Just txAvail_2, Just txAvail_4) => do
          write_heap_bits32 txDesc_8 (cast pktLen)

          -- avail ring[0] = descriptor 0
          write_heap_bits16 txAvail_4 0

          -- Bump avail idx
          let newIdx = txIdx + 1
          write_heap_bits16 txAvail_2 newIdx
          writeNetField16 st TxAvailIdx newIdx

          -- Notify queue 1
          writeVirtIO dev QueueNotify 1
        _ => println "Net: sendTx offset out of heap bounds"
    _ => println "Net: sendTx heap addr out of bounds"

------------------------------------------------------------------------
-- ARP handling
------------------------------------------------------------------------

sendArpReply : HeapAddr -> VirtIODevice -> IO ()
sendArpReply st dev = do
  txBuf   <- readNetField st TxBufferAddr
  peerIp  <- readNetField32 st PeerIp

  -- Zero out VirtIO net header (10 bytes)
  writeZeros txBuf 10

  let eth = txBuf + 10
  arpAddr <- writeEthernetHeader eth $ MkEthernet
    { dstMac    = netMacAddr st PeerMac
    , srcMac    = netMacAddr st OurMac
    , ethertype = 0x0806
    }

  writeARPPacket arpAddr $ MkARP
    { htype = 0x0001
    , ptype = 0x0800
    , hlen  = 6
    , plen  = 4
    , oper  = 0x0002              -- reply
    , sha   = netMacAddr st OurMac
    , spa   = 0x0A00020F          -- 10.0.2.15
    , tha   = netMacAddr st PeerMac
    , tpa   = peerIp
    }

  -- Total: 10 (virtio hdr) + 14 (eth) + 28 (arp) = 52 bytes
  sendTx st dev 52

handleArp : HeapAddr -> VirtIODevice -> Bits64 -> IO ()
handleArp st dev frameStart = do
  arp <- parseARPPacket (frameStart + 14)
  println $ "Net: ARP oper=" ++ b64ToHexString (cast arp.oper) ++ " tpa=0x" ++ b64ToHexString (cast arp.tpa)
  -- Only respond to ARP requests for our IP
  when (arp.oper == 1 && arp.tpa == 0x0A00020F) $ do
    println "Net: ARP request for us, sending reply"
    -- Save sender IP (stored as BE for direct use in IP packet construction)
    writeBE32 (getHeapAddr st + netFieldOffset PeerIp) arp.spa
    -- Save peer MAC from Ethernet src
    copyMac (netMacAddr st PeerMac) (frameStart + 6)
    sendArpReply st dev

------------------------------------------------------------------------
-- Our IP constant
------------------------------------------------------------------------

OUR_IP : Bits32
OUR_IP = 0x0A00020F   -- 10.0.2.15

------------------------------------------------------------------------
-- SYN-ACK construction
------------------------------------------------------------------------

sendSynAck : HeapAddr -> VirtIODevice -> IO ()
sendSynAck st dev = do
  txBuf    <- readNetField st TxBufferAddr
  peerIp   <- readBE32 (getHeapAddr st + netFieldOffset PeerIp)
  peerPort <- readNetField16 st PeerPort
  peerSeqN <- readNetField32 st PeerSeqNext
  ourSeq   <- readNetField32 st OurSeq

  -- Zero VirtIO net header
  writeZeros txBuf 10

  let eth = txBuf + 10
  ipAddr <- writeEthernetHeader eth $ MkEthernet
    { dstMac    = netMacAddr st PeerMac
    , srcMac    = netMacAddr st OurMac
    , ethertype = 0x0800
    }

  tcpAddr <- writeIPv4Header ipAddr $ MkIPv4
    { totalLen = 40               -- 20 (IP) + 20 (TCP)
    , ident    = 0
    , flags    = 0
    , ttl      = 64
    , protocol = 6                -- TCP
    , srcIp    = OUR_IP
    , dstIp    = peerIp
    }

  _ <- writeTCPHeader tcpAddr $ MkTCP
    { srcPort    = 80
    , dstPort    = peerPort
    , seqNum     = ourSeq
    , ackNum     = peerSeqN
    , dataOffset = 0x50           -- 5 * 4 = 20 bytes
    , flags      = 0x12           -- SYN|ACK
    , window     = 65535
    , urgent     = 0
    }

  patchIPChecksum ipAddr
  let scratch = txBuf + 2048
  patchTCPChecksum scratch OUR_IP peerIp 20 tcpAddr

  -- Update our_seq += 1
  writeNetField32 st OurSeq (ourSeq + 1)

  -- Total = 10 + 14 + 20 + 20 = 64 bytes
  sendTx st dev 64

------------------------------------------------------------------------
-- HTTP response
------------------------------------------------------------------------

sendHttpResponse : HeapAddr -> VirtIODevice -> Bits64 -> IO ()
sendHttpResponse st dev payloadLen = do
  txBuf    <- readNetField st TxBufferAddr
  peerIp   <- readBE32 (getHeapAddr st + netFieldOffset PeerIp)
  peerPort <- readNetField16 st PeerPort
  peerSeqN <- readNetField32 st PeerSeqNext
  ourSeq   <- readNetField32 st OurSeq

  -- Zero VirtIO net header
  writeZeros txBuf 10

  let eth = txBuf + 10

  -- HTTP response: "HTTP/1.1 200 OK\r\nContent-Length: 11\r\nConnection: close\r\n\r\nHello World"
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
      httpLen : Bits64 = 69

  let tcpLen : Bits16 = cast (20 + httpLen)  -- TCP header + payload
      ipTotal : Bits16 = cast (20 + 20 + httpLen)  -- IP header + TCP header + payload
      newPeerSeq = peerSeqN + cast payloadLen

  ipAddr <- writeEthernetHeader eth $ MkEthernet
    { dstMac    = netMacAddr st PeerMac
    , srcMac    = netMacAddr st OurMac
    , ethertype = 0x0800
    }

  tcpAddr <- writeIPv4Header ipAddr $ MkIPv4
    { totalLen = ipTotal
    , ident    = 0
    , flags    = 0
    , ttl      = 64
    , protocol = 6
    , srcIp    = OUR_IP
    , dstIp    = peerIp
    }

  payloadAddr <- writeTCPHeader tcpAddr $ MkTCP
    { srcPort    = 80
    , dstPort    = peerPort
    , seqNum     = ourSeq
    , ackNum     = newPeerSeq
    , dataOffset = 0x50           -- 5 * 4 = 20 bytes
    , flags      = 0x19           -- FIN|PSH|ACK
    , window     = 65535
    , urgent     = 0
    }

  -- Write HTTP payload
  writeByteList payloadAddr httpResp

  patchIPChecksum ipAddr
  let scratch = txBuf + 2048
  patchTCPChecksum scratch OUR_IP peerIp tcpLen tcpAddr

  -- Update state
  writeNetField32 st PeerSeqNext newPeerSeq
  writeNetField32 st OurSeq (ourSeq + cast httpLen)

  -- Total = 10 + 14 + 20 + 20 + httpLen
  sendTx st dev (10 + 14 + 20 + 20 + cast httpLen)

------------------------------------------------------------------------
-- HTTP payload printing
------------------------------------------------------------------------

printPayload : Bits64 -> Bits64 -> IO ()
printPayload addr len = do
  println "=== HTTP REQUEST ==="
  let go : Bits64 -> IO ()
      go i =
        if i >= len
          then pure ()
          else do
            c <- readByte (addr + i)
            if (c == 0x0A || c == 0x0D || (c >= 0x20 && c < 0x7F))
              then write_mmio_bits8 UART c
              else write_mmio_bits8 UART (cast '.')
            go (i + 1)
  go 0
  println "\n==="

------------------------------------------------------------------------
-- TCP handling
------------------------------------------------------------------------

handleTcp : HeapAddr -> VirtIODevice -> Bits64 -> Bits64 -> IO ()
handleTcp st dev ipStart tcpStart = do
  tcp <- parseTCPHeader tcpStart
  println $ "Net: TCP dstPort=" ++ b64ToHexString (cast tcp.dstPort) ++ " flags=0x" ++ b64ToHexString (cast tcp.flags)
  when (tcp.dstPort == 80) $ do
    let isSyn = tcp.flags .&. 0x02
        isAck = tcp.flags .&. 0x10
        isPsh = tcp.flags .&. 0x08
    if (isSyn /= 0 && isAck == 0)
      then do
        println "Net: TCP SYN received, sending SYN-ACK"
        -- Save peer port
        writeNetField16 st PeerPort tcp.srcPort
        -- Save peer IP from IP header (store as BE for IP packet construction)
        peerIp <- readBE32 (ipStart + 12)
        writeBE32 (getHeapAddr st + netFieldOffset PeerIp) peerIp
        -- peer_seq_next = seq + 1
        writeNetField32 st PeerSeqNext (tcp.seqNum + 1)
        sendSynAck st dev
        println "Net: SYN-ACK sent"
      else if isPsh /= 0
        then do
          println "Net: TCP PSH received, printing payload"
          let payloadStart = tcpStart + tcp.dataOffset
          -- IP total length from IP header
          ip <- parseIPv4Header ipStart
          let tcpSegLen : Bits64 = cast ip.totalLen - ip.ihl
              payloadLen = tcpSegLen - tcp.dataOffset
          println $ "Net: payloadLen=" ++ b64ToHexString payloadLen
          when (payloadLen > 0) $ do
            printPayload payloadStart payloadLen
            sendHttpResponse st dev payloadLen
            println "Net: HTTP response sent"
        else println $ "Net: TCP no-op flags=0x" ++ b64ToHexString (cast tcp.flags)

------------------------------------------------------------------------
-- IPv4 handling
------------------------------------------------------------------------

handleIPv4 : HeapAddr -> VirtIODevice -> Bits64 -> Bits64 -> IO ()
handleIPv4 st dev frameStart frameLen = do
  let ipAddr = frameStart + 14
  ip <- parseIPv4Header ipAddr
  println $ "Net: IPv4 proto=" ++ b64ToHexString (cast ip.protocol) ++ " dstIp=0x" ++ b64ToHexString (cast ip.dstIp)
  when (ip.protocol == 6 && ip.dstIp == OUR_IP) $ do
    let tcpAddr = ipAddr + ip.ihl
    handleTcp st dev ipAddr tcpAddr

------------------------------------------------------------------------
-- Frame dispatch
------------------------------------------------------------------------

dispatchFrame : HeapAddr -> VirtIODevice -> Bits64 -> Bits64 -> IO ()
dispatchFrame st dev frameStart frameLen = do
  eth <- parseEthernetHeader frameStart
  println $ "Net: ethertype=0x" ++ b64ToHexString (cast eth.ethertype) ++ " frameLen=" ++ b64ToHexString frameLen
  case eth.ethertype of
    0x0806 => handleArp  st dev frameStart
    0x0800 => handleIPv4 st dev frameStart frameLen
    _      => println $ "Net: unknown ethertype, ignoring"

------------------------------------------------------------------------
-- IRQ handler
------------------------------------------------------------------------

export
handleNetIrq : IO ()
handleNetIrq = do
  stateRaw <- primIO prim__read_mscratch
  if stateRaw == 0
    then pure ()
    else case mkHeapAddr stateRaw of
      Nothing => println "Net: IRQ state addr out of heap bounds"
      Just st => do
        baseRaw <- readNetField st VirtioBase
        case mkVirtIODevice baseRaw of
          Nothing  => println "Net: IRQ virtio base out of range"
          Just dev => do
            availRaw  <- readNetField st RxAvailAddr
            idx       <- readNetField16 st RxAvailIdx
            rxUsedRaw <- readNetField st RxUsedAddr
            rxBuf     <- readNetField st RxBufferAddr

            case (mkHeapAddr availRaw, mkHeapAddr rxUsedRaw) of
              (Just availH, Just rxUsedH) => do
                -- 1. Ack the virtio interrupt
                isr    <- readVirtIO dev InterruptStatus
                status <- readVirtIO dev Status
                println $ "Net: IRQ isr=0x" ++ b64ToHexString (cast isr) ++ " status=0x" ++ b64ToHexString (cast status)
                writeVirtIO dev InterruptACK isr

                -- 2. Check if there's a used descriptor (used_idx at offset 2)
                Just rxUsed_2 <- pure $ offsetHeapAddr rxUsedH 2
                  | Nothing => println "Net: rxUsed+2 out of heap bounds"
                Just rxUsed_8 <- pure $ offsetHeapAddr rxUsedH 8
                  | Nothing => println "Net: rxUsed+8 out of heap bounds"
                usedIdx  <- read_heap_bits16 rxUsed_2
                lastUsed <- readNetField16 st LastUsedIdx
                println $ "Net: idx=" ++ b64ToHexString (cast idx) ++ " usedIdx=" ++ b64ToHexString (cast usedIdx) ++ " lastUsed=" ++ b64ToHexString (cast lastUsed) ++ " rxUsed=0x" ++ b64ToHexString rxUsedRaw
                if usedIdx > lastUsed
                  then do
                    -- Read received packet length from used ring (used+8 = ring[0].len)
                    pktLen32 <- read_heap_bits32 rxUsed_8
                    println $ "Net: pktLen=" ++ b64ToHexString (cast pktLen32)
                    when (pktLen32 > 10) $ do
                      let frameStart = rxBuf + 10           -- skip 10-byte VirtIO net header
                          frameLen   : Bits64 = cast pktLen32 - 10
                      println $ "Net: frameLen=" ++ b64ToHexString frameLen
                      when (frameLen > 14) $
                        dispatchFrame st dev frameStart frameLen

                    -- 3. Update lastUsed index
                    writeNetField16 st LastUsedIdx usedIdx

                    -- 4. Re-arm: put descriptor 0 back in available ring
                    Just avail_2 <- pure $ offsetHeapAddr availH 2
                      | Nothing => println "Net: avail+2 out of heap bounds"
                    Just avail_4 <- pure $ offsetHeapAddr availH 4
                      | Nothing => println "Net: avail+4 out of heap bounds"
                    let newAvailIdx = idx + 1
                    write_heap_bits16 avail_4 0           -- ring[0] = descriptor 0
                    write_heap_bits16 avail_2 newAvailIdx -- bump avail.idx
                    writeNetField16 st RxAvailIdx newAvailIdx
                    writeVirtIO dev QueueNotify 0         -- notify queue 0

                    println $ "Net: processed usedIdx=" ++ b64ToHexString (cast usedIdx)
                  else do
                    println "Net: no new used descriptor"
              _ => println "Net: IRQ avail/rxUsed addr out of heap bounds"


------------------------------------------------------------------------
-- Allocate a page and identity-map it
------------------------------------------------------------------------

export
allocAndMap : {numPages : Nat} -> (0 _ : LT 1 numPages) => Kernel numPages (Either AllocPagesErrors HeapAddr)
allocAndMap = do
  Right page <- liftInit $ zalloc (mkNatPos 1)
    | Left err => pure (Left err)
  Right () <- mmap (getHeapAddr page) (getHeapAddr page) entryBits.ReadWrite
    | Left err => pure (Left err)
  pure (Right page)

------------------------------------------------------------------------
-- Network setup
------------------------------------------------------------------------

export
setupNetwork : {numPages : Nat} -> VirtIODevice -> Kernel numPages ()
setupNetwork dev = do
  case isLT 1 numPages of
    No _ => println "Page table too small for network setup"
    Yes prf => do
      Right rx_vq     <- allocAndMap @{prf}
        | Left err => println $ "Net: alloc rx_vq failed: " ++ show err
      Right rx_buffer <- allocAndMap @{prf}
        | Left err => println $ "Net: alloc rx_buffer failed: " ++ show err

      println "Setup network"

      Right rx_used_pg <- allocAndMap @{prf}
        | Left err => println $ "Net: alloc rx_used failed: " ++ show err

      -- TX allocations
      Right tx_vq      <- allocAndMap @{prf}
        | Left err => println $ "Net: alloc tx_vq failed: " ++ show err
      Right tx_buffer  <- allocAndMap @{prf}
        | Left err => println $ "Net: alloc tx_buffer failed: " ++ show err
      Right tx_used_pg <- allocAndMap @{prf}
        | Left err => println $ "Net: alloc tx_used failed: " ++ show err

      -- Allocate a state page for the IRQ handler
      Right statePg <- allocAndMap @{prf}
        | Left err => println $ "Net: alloc state page failed: " ++ show err

      -- Compute heap-bounded offset addresses for descriptor/ring structures.
      -- These are small offsets within 4096-byte pages, so offsetHeapAddr always succeeds.
      let desc    = rx_vq
          buffer  = getHeapAddr rx_buffer
      Just rxAvail_0 <- pure $ offsetHeapAddr rx_vq 16
        | Nothing => println "Net: offset rx avail out of heap bounds"
      Just rxAvail_2 <- pure $ offsetHeapAddr rx_vq 18
        | Nothing => println "Net: offset rx avail+2 out of heap bounds"
      Just rxAvail_4 <- pure $ offsetHeapAddr rx_vq 20
        | Nothing => println "Net: offset rx avail+4 out of heap bounds"
      Just rxUsed_0 <- pure $ offsetHeapAddr rx_vq 24
        | Nothing => println "Net: offset rx used out of heap bounds"
      Just rxUsed_2 <- pure $ offsetHeapAddr rx_vq 26
        | Nothing => println "Net: offset rx used+2 out of heap bounds"
      Just desc_8  <- pure $ offsetHeapAddr rx_vq 8
        | Nothing => println "Net: offset desc+8 out of heap bounds"
      Just desc_12 <- pure $ offsetHeapAddr rx_vq 12
        | Nothing => println "Net: offset desc+12 out of heap bounds"
      Just desc_14 <- pure $ offsetHeapAddr rx_vq 14
        | Nothing => println "Net: offset desc+14 out of heap bounds"

      let txDesc   = tx_vq
      Just txDesc_8  <- pure $ offsetHeapAddr tx_vq 8
        | Nothing => println "Net: offset txDesc+8 out of heap bounds"
      Just txDesc_12 <- pure $ offsetHeapAddr tx_vq 12
        | Nothing => println "Net: offset txDesc+12 out of heap bounds"
      Just txDesc_14 <- pure $ offsetHeapAddr tx_vq 14
        | Nothing => println "Net: offset txDesc+14 out of heap bounds"
      Just txAvail_0 <- pure $ offsetHeapAddr tx_vq 16
        | Nothing => println "Net: offset tx avail out of heap bounds"
      Just txAvail_2 <- pure $ offsetHeapAddr tx_vq 18
        | Nothing => println "Net: offset tx avail+2 out of heap bounds"
      Just txAvail_4 <- pure $ offsetHeapAddr tx_vq 20
        | Nothing => println "Net: offset tx avail+4 out of heap bounds"

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
      -- Set QueueAlign to 4 so used ring fits in same page as descriptors
      writeVirtIO dev QueueAlign 4

      -- RX Descriptor 0 (WRITE flag = 2, device writes to buffer)
      write_heap_bits64 desc buffer
      write_heap_bits32 desc_8 2048
      write_heap_bits16 desc_12 2
      write_heap_bits16 desc_14 0
      println $ "Net: RX desc at 0x" ++ b64ToHexString (getHeapAddr desc) ++ " buffer=0x" ++ b64ToHexString buffer

      -- RX Available ring: flags=0, idx=1, ring[0]=0
      write_heap_bits16 rxAvail_0 0
      write_heap_bits16 rxAvail_2 1
      write_heap_bits16 rxAvail_4 0
      println $ "Net: RX avail ring at 0x" ++ b64ToHexString (getHeapAddr rxAvail_0) ++ " idx=1 ring[0]=0"

      -- RX Used ring
      write_heap_bits16 rxUsed_0 0
      write_heap_bits16 rxUsed_2 0

      -- QueuePFN for RX
      writeVirtIO dev QueuePFN (cast $ shiftR (getHeapAddr rx_vq) 12)

      -- ---- TX Queue (queue 1) ----
      writeVirtIO dev QueueSel 1
      writeVirtIO dev QueueNum 1
      writeVirtIO dev QueueAlign 4

      -- TX Descriptor 0 (flags=0, driver writes to buffer)
      write_heap_bits64 txDesc (getHeapAddr tx_buffer)
      write_heap_bits32 txDesc_8 2048
      write_heap_bits16 txDesc_12 0
      write_heap_bits16 txDesc_14 0

      -- TX Available ring: idx=0
      write_heap_bits16 txAvail_0 0
      write_heap_bits16 txAvail_2 0
      write_heap_bits16 txAvail_4 0

      -- QueuePFN for TX
      writeVirtIO dev QueuePFN (cast $ shiftR (getHeapAddr tx_vq) 12)

      -- DRIVER_OK
      writeVirtIO dev Status 7

      -- Notify device that RX descriptor is available
      writeVirtIO dev QueueNotify 0
      println "Net: notified device of RX descriptor"

      -- Read MAC from VirtIO config space
      (mac0, mac1, mac2, mac3, mac4, mac5) <- readVirtIOMAC dev
      println $ "Net: MAC=" ++ b64ToHexString (cast mac0) ++ ":" ++ b64ToHexString (cast mac1) ++ ":" ++ b64ToHexString (cast mac2) ++ ":" ++ b64ToHexString (cast mac3) ++ ":" ++ b64ToHexString (cast mac4) ++ ":" ++ b64ToHexString (cast mac5)

      -- Write state page using typed field accessors
      writeNetField statePg VirtioBase (getVirtIOAddr dev)
      writeNetField statePg RxAvailAddr (getHeapAddr rxAvail_0)
      writeNetField16 statePg RxAvailIdx 1
      writeNetField16 statePg LastUsedIdx 0
      writeNetField statePg RxUsedAddr (getHeapAddr rxUsed_0)
      writeNetField statePg RxBufferAddr buffer
      writeNetField statePg TxDescAddr (getHeapAddr txDesc)
      writeNetField statePg TxAvailAddr (getHeapAddr txAvail_0)
      writeNetField statePg TxBufferAddr (getHeapAddr tx_buffer)
      writeNetField16 statePg TxAvailIdx 0
      -- our_mac
      writeNetMAC statePg OurMac mac0 mac1 mac2 mac3 mac4 mac5
      -- our_seq
      writeNetField32 statePg OurSeq 0x1000
      -- rest: peer_seq_next, peer_port, peer_ip start as 0
      writeNetField32 statePg PeerSeqNext 0
      writeNetField16 statePg PeerPort 0
      writeNetField32 statePg PeerIp 0

      -- Set mscratch to state page address via ecall into M-mode
      primIO $ prim__make_syscall (getHeapAddr statePg)

      println "Virtio network setup complete."
