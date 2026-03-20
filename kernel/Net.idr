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

-- State page layout:
--   offset 0:   virtio_base          (Bits64)
--   offset 8:   rx_avail_addr        (Bits64)
--   offset 16:  rx_avail_idx         (Bits16)
--   offset 18:  _pad (6 bytes)
--   offset 24:  rx_used_addr         (Bits64)
--   offset 32:  rx_buffer_addr       (Bits64)
--   offset 40:  tx_desc_addr         (Bits64)
--   offset 48:  tx_avail_addr        (Bits64)
--   offset 56:  tx_buffer_addr       (Bits64)
--   offset 64:  tx_avail_idx         (Bits16)
--   offset 66:  _pad (6 bytes)
--   offset 72:  our_mac              (6 bytes)
--   offset 78:  _pad (2 bytes)
--   offset 80:  our_seq              (Bits32)
--   offset 84:  peer_seq_next        (Bits32)
--   offset 88:  peer_port            (Bits16)
--   offset 90:  _pad (2 bytes)
--   offset 92:  peer_ip              (Bits32)
--   offset 96:  peer_mac             (6 bytes)
--   offset 102: _pad (2 bytes)

------------------------------------------------------------------------
-- Byte-order helpers
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

------------------------------------------------------------------------
-- Utilities
------------------------------------------------------------------------

copyMac : Bits64 -> Bits64 -> IO ()
copyMac dst src = do
  let go : Bits64 -> IO ()
      go 6 = pure ()
      go i = do
        b <- primIO $ prim__deref_bits8 (src + i)
        primIO $ prim__set_bits8 (dst + i) b
        go (i + 1)
  go 0

writeZeros : Bits64 -> Bits64 -> IO ()
writeZeros addr n = do
  let go : Bits64 -> IO ()
      go 0 = pure ()
      go i = do
        primIO $ prim__set_bits8 (addr + (n - i)) 0
        go (i - 1)
  go n

------------------------------------------------------------------------
-- Internet checksum
------------------------------------------------------------------------

-- Accumulate one's-complement sum over n bytes at addr
-- Returns the running 32-bit accumulator (to allow combining multiple regions)
onesCompSum : Bits64 -> Bits64 -> IO Bits32
onesCompSum addr n = go 0 0
  where
    go : Bits64 -> Bits32 -> IO Bits32
    go i acc =
      if i >= n
        then pure acc
        else if i + 1 < n
          then do
            hi <- primIO $ prim__deref_bits8 (addr + i)
            lo <- primIO $ prim__deref_bits8 (addr + i + 1)
            let w : Bits32 = (cast hi `shiftL` 8) .|. cast lo
            go (i + 2) (acc + w)
          else do
            hi <- primIO $ prim__deref_bits8 (addr + i)
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

-- tcpChecksum: build pseudo-header in scratch (12 bytes), sum with TCP segment
tcpChecksum : Bits64 -> Bits32 -> Bits32 -> Bits16 -> Bits64 -> IO Bits16
tcpChecksum scratch srcIp dstIp tcpLen tcpAddr = do
  -- Pseudo-header: src_ip(4) dst_ip(4) zero(1) proto=6(1) tcp_len(2)
  writeBE32 scratch srcIp
  writeBE32 (scratch + 4) dstIp
  primIO $ prim__set_bits8 (scratch + 8) 0
  primIO $ prim__set_bits8 (scratch + 9) 6
  writeBE16 (scratch + 10) tcpLen
  acc1 <- onesCompSum scratch 12
  acc2 <- onesCompSum tcpAddr (cast tcpLen)
  pure (foldChecksum (acc1 + acc2))

------------------------------------------------------------------------
-- TX send helper
------------------------------------------------------------------------

sendTx : Bits64 -> Bits64 -> Bits64 -> IO ()
sendTx stateAddr base pktLen = do
  txDesc  <- primIO $ prim__deref_bits64 (stateAddr + 40)
  txAvail <- primIO $ prim__deref_bits64 (stateAddr + 48)
  txIdx   <- primIO $ prim__deref_bits16 (stateAddr + 64)

  -- Update descriptor length
  primIO $ prim__set_bits32 (txDesc + 8) (cast pktLen)

  -- avail ring[0] = descriptor 0
  primIO $ prim__set_bits16 (txAvail + 4) 0

  -- Bump avail idx
  let newIdx = txIdx + 1
  primIO $ prim__set_bits16 (txAvail + 2) newIdx
  primIO $ prim__set_bits16 (stateAddr + 64) newIdx

  -- Notify queue 1
  primIO $ prim__set_bits32 (base + 0x50) 1

------------------------------------------------------------------------
-- ARP handling
------------------------------------------------------------------------

sendArpReply : Bits64 -> Bits64 -> IO ()
sendArpReply stateAddr base = do
  txBuf   <- primIO $ prim__deref_bits64 (stateAddr + 56)
  peerIp  <- primIO $ prim__deref_bits32 (stateAddr + 92)

  -- Zero out VirtIO net header (10 bytes)
  writeZeros txBuf 10

  let eth = txBuf + 10

  -- Ethernet dst = peer MAC
  copyMac eth (stateAddr + 96)
  -- Ethernet src = our MAC
  copyMac (eth + 6) (stateAddr + 72)
  -- Ethertype = 0x0806 (ARP)
  writeBE16 (eth + 12) 0x0806

  let arp = eth + 14

  -- ARP fields
  writeBE16 arp        0x0001  -- htype = Ethernet
  writeBE16 (arp + 2)  0x0800  -- ptype = IPv4
  primIO $ prim__set_bits8 (arp + 4) 6  -- hlen
  primIO $ prim__set_bits8 (arp + 5) 4  -- plen
  writeBE16 (arp + 6)  0x0002  -- oper = reply

  -- sha = our MAC
  copyMac (arp + 8) (stateAddr + 72)
  -- spa = 10.0.2.15 = 0x0A00020F
  writeBE32 (arp + 14) 0x0A00020F
  -- tha = sender MAC
  copyMac (arp + 18) (stateAddr + 96)
  -- tpa = sender IP
  writeBE32 (arp + 24) peerIp

  -- Total: 10 (virtio hdr) + 14 (eth) + 28 (arp) = 52 bytes
  sendTx stateAddr base 52

handleArp : Bits64 -> Bits64 -> Bits64 -> IO ()
handleArp stateAddr base frameStart = do
  let arp = frameStart + 14
  oper <- readBE16 (arp + 6)
  tpa  <- readBE32 (arp + 24)
  println $ "Net: ARP oper=" ++ b64ToHexString (cast oper) ++ " tpa=0x" ++ b64ToHexString (cast tpa)
  -- Only respond to ARP requests for our IP
  when (oper == 1 && tpa == 0x0A00020F) $ do
    println "Net: ARP request for us, sending reply"
    -- Save sender IP
    senderIp <- readBE32 (arp + 14)
    primIO $ prim__set_bits32 (stateAddr + 92) senderIp   -- little-endian store
    -- Actually store as BE in state for later use in IP packets
    writeBE32 (stateAddr + 92) senderIp
    -- Save sender MAC (6 bytes from arp+8)
    copyMac (stateAddr + 96) (arp + 8)
    -- Also save peer MAC from Ethernet src
    copyMac (stateAddr + 96) (frameStart + 6)
    sendArpReply stateAddr base

------------------------------------------------------------------------
-- SYN-ACK construction
------------------------------------------------------------------------

sendSynAck : Bits64 -> Bits64 -> IO ()
sendSynAck stateAddr base = do
  txBuf    <- primIO $ prim__deref_bits64 (stateAddr + 56)
  peerIp   <- readBE32 (stateAddr + 92)
  peerPort <- primIO $ prim__deref_bits16 (stateAddr + 88)
  peerSeqN <- primIO $ prim__deref_bits32 (stateAddr + 84)
  ourSeq   <- primIO $ prim__deref_bits32 (stateAddr + 80)

  -- Zero VirtIO net header
  writeZeros txBuf 10

  let eth = txBuf + 10
      ip  = eth + 14
      tcp = ip + 20

  -- Ethernet header
  copyMac eth (stateAddr + 96)         -- dst = peer MAC
  copyMac (eth + 6) (stateAddr + 72)   -- src = our MAC
  writeBE16 (eth + 12) 0x0800          -- type = IPv4

  -- IPv4 header
  primIO $ prim__set_bits8 ip       0x45  -- version=4, IHL=5
  primIO $ prim__set_bits8 (ip + 1) 0x00  -- DSCP/ECN
  writeBE16 (ip + 2) 40                   -- total length = 20+20
  writeBE16 (ip + 4) 0                    -- identification
  writeBE16 (ip + 6) 0                    -- flags/fragment offset
  primIO $ prim__set_bits8 (ip + 8) 64    -- TTL
  primIO $ prim__set_bits8 (ip + 9) 6     -- protocol = TCP
  writeBE16 (ip + 10) 0                   -- checksum (fill later)
  writeBE32 (ip + 12) 0x0A00020F          -- src = 10.0.2.15
  writeBE32 (ip + 16) peerIp              -- dst = peer IP

  -- TCP header
  writeBE16 tcp        80                 -- sport = 80
  writeBE16 (tcp + 2)  peerPort           -- dport
  writeBE32 (tcp + 4)  ourSeq             -- seq
  writeBE32 (tcp + 8)  peerSeqN          -- ack = peer_seq_next
  primIO $ prim__set_bits8 (tcp + 12) 0x50  -- data offset = 5 (20 bytes)
  primIO $ prim__set_bits8 (tcp + 13) 0x12  -- flags = SYN|ACK
  writeBE16 (tcp + 14) 65535              -- window
  writeBE16 (tcp + 16) 0                  -- checksum (fill later)
  writeBE16 (tcp + 18) 0                  -- urgent

  -- IP checksum
  ipCsum <- ipChecksum ip 20
  writeBE16 (ip + 10) ipCsum

  -- TCP checksum using scratch space after tx buffer
  let scratch = txBuf + 2048
  tcpCsum <- tcpChecksum scratch 0x0A00020F peerIp 20 tcp
  writeBE16 (tcp + 16) tcpCsum

  -- Update our_seq += 1
  primIO $ prim__set_bits32 (stateAddr + 80) (ourSeq + 1)

  -- Total = 10 + 14 + 20 + 20 = 64 bytes
  sendTx stateAddr base 64

------------------------------------------------------------------------
-- HTTP response
------------------------------------------------------------------------

sendHttpResponse : Bits64 -> Bits64 -> Bits64 -> IO ()
sendHttpResponse stateAddr base payloadLen = do
  txBuf    <- primIO $ prim__deref_bits64 (stateAddr + 56)
  peerIp   <- readBE32 (stateAddr + 92)
  peerPort <- primIO $ prim__deref_bits16 (stateAddr + 88)
  peerSeqN <- primIO $ prim__deref_bits32 (stateAddr + 84)
  ourSeq   <- primIO $ prim__deref_bits32 (stateAddr + 80)

  -- Zero VirtIO net header
  writeZeros txBuf 10

  let eth = txBuf + 10
      ip  = eth + 14
      tcp = ip + 20
      payload = tcp + 20

  -- HTTP response: "HTTP/1.1 200 OK\r\nContent-Length: 11\r\nConnection: close\r\n\r\nHello World"
  -- That's 67 bytes
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

  -- Write HTTP payload bytes
  let writeBytes : Bits64 -> List Bits8 -> IO ()
      writeBytes _ []        = pure ()
      writeBytes i (b :: bs) = do
        primIO $ prim__set_bits8 (payload + i) b
        writeBytes (i + 1) bs
  writeBytes 0 httpResp

  let tcpLen : Bits16 = cast (20 + httpLen)  -- TCP header + payload
      ipTotal : Bits16 = cast (20 + 20 + httpLen)  -- IP header + TCP header + payload

  -- Ethernet header
  copyMac eth (stateAddr + 96)
  copyMac (eth + 6) (stateAddr + 72)
  writeBE16 (eth + 12) 0x0800

  -- IPv4 header
  primIO $ prim__set_bits8 ip       0x45
  primIO $ prim__set_bits8 (ip + 1) 0x00
  writeBE16 (ip + 2) ipTotal
  writeBE16 (ip + 4) 0
  writeBE16 (ip + 6) 0
  primIO $ prim__set_bits8 (ip + 8) 64
  primIO $ prim__set_bits8 (ip + 9) 6
  writeBE16 (ip + 10) 0
  writeBE32 (ip + 12) 0x0A00020F
  writeBE32 (ip + 16) peerIp

  -- TCP header: ACK the request data, send response with PSH|FIN|ACK
  writeBE16 tcp        80
  writeBE16 (tcp + 2)  peerPort
  writeBE32 (tcp + 4)  ourSeq
  -- ACK peer's data: peer_seq_next + payloadLen
  let newPeerSeq = peerSeqN + cast payloadLen
  writeBE32 (tcp + 8)  newPeerSeq
  primIO $ prim__set_bits8 (tcp + 12) 0x50   -- data offset = 5
  primIO $ prim__set_bits8 (tcp + 13) 0x19   -- flags = FIN|PSH|ACK
  writeBE16 (tcp + 14) 65535
  writeBE16 (tcp + 16) 0
  writeBE16 (tcp + 18) 0

  -- IP checksum
  ipCsum <- ipChecksum ip 20
  writeBE16 (ip + 10) ipCsum

  -- TCP checksum
  let scratch = txBuf + 2048
  tcpCsum <- tcpChecksum scratch 0x0A00020F peerIp tcpLen tcp
  writeBE16 (tcp + 16) tcpCsum

  -- Update state
  primIO $ prim__set_bits32 (stateAddr + 84) newPeerSeq
  primIO $ prim__set_bits32 (stateAddr + 80) (ourSeq + cast httpLen)

  -- Total = 10 + 14 + 20 + 20 + 49 = 113 bytes
  sendTx stateAddr base (10 + 14 + 20 + 20 + cast httpLen)

------------------------------------------------------------------------
-- HTTP payload printing
------------------------------------------------------------------------

printPayload : Bits64 -> Bits64 -> IO ()
printPayload addr len = do
  write_mmio_bits8 UART (cast '=')
  write_mmio_bits8 UART (cast '=')
  write_mmio_bits8 UART (cast '=')
  write_mmio_bits8 UART (cast ' ')
  write_mmio_bits8 UART (cast 'H')
  write_mmio_bits8 UART (cast 'T')
  write_mmio_bits8 UART (cast 'T')
  write_mmio_bits8 UART (cast 'P')
  write_mmio_bits8 UART (cast ' ')
  write_mmio_bits8 UART (cast 'R')
  write_mmio_bits8 UART (cast 'e')
  write_mmio_bits8 UART (cast 'q')
  write_mmio_bits8 UART (cast 'u')
  write_mmio_bits8 UART (cast 'e')
  write_mmio_bits8 UART (cast 's')
  write_mmio_bits8 UART (cast 't')
  write_mmio_bits8 UART (cast ' ')
  write_mmio_bits8 UART (cast '=')
  write_mmio_bits8 UART (cast '=')
  write_mmio_bits8 UART (cast '=')
  write_mmio_bits8 UART (cast '\n')
  let go : Bits64 -> IO ()
      go i =
        if i >= len
          then pure ()
          else do
            b <- primIO $ prim__deref_bits8 (addr + i)
            let c : Bits8 = b
            if (c == 0x0A || c == 0x0D || (c >= 0x20 && c < 0x7F))
              then write_mmio_bits8 UART c
              else write_mmio_bits8 UART (cast '.')
            go (i + 1)
  go 0
  write_mmio_bits8 UART (cast '\n')
  write_mmio_bits8 UART (cast '=')
  write_mmio_bits8 UART (cast '=')
  write_mmio_bits8 UART (cast '=')
  write_mmio_bits8 UART (cast '\n')

------------------------------------------------------------------------
-- TCP handling
------------------------------------------------------------------------

handleTcp : Bits64 -> Bits64 -> Bits64 -> Bits64 -> IO ()
handleTcp stateAddr base ipStart tcpStart = do
  dstPort <- readBE16 (tcpStart + 2)
  flags   <- primIO $ prim__deref_bits8 (tcpStart + 13)
  println $ "Net: TCP dstPort=" ++ b64ToHexString (cast dstPort) ++ " flags=0x" ++ b64ToHexString (cast flags)
  when (dstPort == 80) $ do
    let isSyn = flags .&. 0x02
        isAck = flags .&. 0x10
        isPsh = flags .&. 0x08
    if (isSyn /= 0 && isAck == 0)
      then do
        println "Net: TCP SYN received, sending SYN-ACK"
        -- Save peer port
        srcPort <- readBE16 tcpStart
        primIO $ prim__set_bits16 (stateAddr + 88) srcPort
        -- Save peer IP from IP header
        peerIp <- readBE32 (ipStart + 12)
        writeBE32 (stateAddr + 92) peerIp
        -- Save peer MAC from state+96 (already set by ARP or from eth src)
        -- peer_seq_next = seq + 1
        seqNum <- readBE32 (tcpStart + 4)
        primIO $ prim__set_bits32 (stateAddr + 84) (seqNum + 1)
        sendSynAck stateAddr base
        println "Net: SYN-ACK sent"
      else if isPsh /= 0
        then do
          println "Net: TCP PSH received, printing payload"
          -- Compute TCP header length from data offset nibble
          dataOffByte <- primIO $ prim__deref_bits8 (tcpStart + 12)
          let tcpHdrLen : Bits64 = cast ((dataOffByte `shiftR` 4) * 4)
              payloadStart = tcpStart + tcpHdrLen
          -- IP total length from IP header
          ipTotalLen <- readBE16 (ipStart + 2)
          let ipHdrLen : Bits64 = cast ((!(primIO $ prim__deref_bits8 ipStart) .&. 0x0F) * 4)
              tcpSegLen : Bits64 = cast ipTotalLen - ipHdrLen
              payloadLen = tcpSegLen - tcpHdrLen
          println $ "Net: payloadLen=" ++ b64ToHexString payloadLen
          when (payloadLen > 0) $ do
            printPayload payloadStart payloadLen
            sendHttpResponse stateAddr base payloadLen
            println "Net: HTTP response sent"
        else println $ "Net: TCP no-op flags=0x" ++ b64ToHexString (cast flags)

------------------------------------------------------------------------
-- IPv4 handling
------------------------------------------------------------------------

handleIPv4 : Bits64 -> Bits64 -> Bits64 -> Bits64 -> IO ()
handleIPv4 stateAddr base frameStart frameLen = do
  let ip = frameStart + 14
  proto  <- primIO $ prim__deref_bits8 (ip + 9)
  dstIp  <- readBE32 (ip + 16)
  println $ "Net: IPv4 proto=" ++ b64ToHexString (cast proto) ++ " dstIp=0x" ++ b64ToHexString (cast dstIp)
  when (proto == 6 && dstIp == 0x0A00020F) $ do
    ihlByte <- primIO $ prim__deref_bits8 ip
    let ihl : Bits64 = cast ((ihlByte .&. 0x0F) * 4)
        tcp = ip + ihl
    handleTcp stateAddr base ip tcp

------------------------------------------------------------------------
-- Frame dispatch
------------------------------------------------------------------------

dispatchFrame : Bits64 -> Bits64 -> Bits64 -> Bits64 -> IO ()
dispatchFrame stateAddr base frameStart frameLen = do
  ethertype <- readBE16 (frameStart + 12)
  println $ "Net: ethertype=0x" ++ b64ToHexString (cast ethertype) ++ " frameLen=" ++ b64ToHexString frameLen
  case ethertype of
    0x0806 => handleArp  stateAddr base frameStart
    0x0800 => handleIPv4 stateAddr base frameStart frameLen
    _      => println $ "Net: unknown ethertype, ignoring"

------------------------------------------------------------------------
-- IRQ handler
------------------------------------------------------------------------

export
handleNetIrq : IO ()
handleNetIrq = do
  stateAddr <- primIO prim__read_mscratch
  if stateAddr == 0
    then pure ()
    else do
      base   <- primIO $ prim__deref_bits64 stateAddr
      avail  <- primIO $ prim__deref_bits64 (stateAddr + 8)
      idx    <- primIO $ prim__deref_bits16 (stateAddr + 16)
      rxUsed <- primIO $ prim__deref_bits64 (stateAddr + 24)
      rxBuf  <- primIO $ prim__deref_bits64 (stateAddr + 32)

      -- 1. Ack the virtio interrupt
      isr <- primIO $ prim__deref_bits32 (base + 0x60)
      status <- primIO $ prim__deref_bits32 (base + 0x70)
      println $ "Net: IRQ isr=0x" ++ b64ToHexString (cast isr) ++ " status=0x" ++ b64ToHexString (cast status)
      primIO $ prim__set_bits32 (base + 0x64) isr

      -- 2. Check if there's a used descriptor (used_idx at offset 2)
      usedIdx <- primIO $ prim__deref_bits16 (rxUsed + 2)
      lastUsed <- primIO $ prim__deref_bits16 (stateAddr + 18)
      println $ "Net: idx=" ++ b64ToHexString (cast idx) ++ " usedIdx=" ++ b64ToHexString (cast usedIdx) ++ " lastUsed=" ++ b64ToHexString (cast lastUsed) ++ " rxUsed=0x" ++ b64ToHexString rxUsed
      if usedIdx > lastUsed
        then do
          -- Read received packet length from used ring (used+8 = ring[0].len)
          pktLen32 <- primIO $ prim__deref_bits32 (rxUsed + 8)
          println $ "Net: pktLen=" ++ b64ToHexString (cast pktLen32)
          when (pktLen32 > 10) $ do
            let frameStart = rxBuf + 10           -- skip 10-byte VirtIO net header
                frameLen   : Bits64 = cast pktLen32 - 10
            println $ "Net: frameLen=" ++ b64ToHexString frameLen
            when (frameLen > 14) $
              dispatchFrame stateAddr base frameStart frameLen

          -- 3. Update lastUsed index
          primIO $ prim__set_bits16 (stateAddr + 18) usedIdx

          -- 4. Re-arm: put descriptor 0 back in available ring
          let newAvailIdx = idx + 1
          primIO $ prim__set_bits16 (avail + 4) 0           -- ring[0] = descriptor 0
          primIO $ prim__set_bits16 (avail + 2) newAvailIdx -- bump avail.idx
          primIO $ prim__set_bits16 (stateAddr + 16) newAvailIdx
          primIO $ prim__set_bits32 (base + 0x50) 0         -- notify queue 0

          println $ "Net: processed usedIdx=" ++ b64ToHexString (cast usedIdx)
        else do
          println "Net: no new used descriptor"

export
pollNet : IO ()
pollNet = do
  stateAddr <- primIO prim__read_mscratch
  if stateAddr == 0
    then println "Net: pollNet - no state"
    else do
      base   <- primIO $ prim__deref_bits64 stateAddr
      avail  <- primIO $ prim__deref_bits64 (stateAddr + 8)
      idx    <- primIO $ prim__deref_bits16 (stateAddr + 16)
      rxUsed <- primIO $ prim__deref_bits64 (stateAddr + 24)
      rxBuf  <- primIO $ prim__deref_bits64 (stateAddr + 32)
      lastUsed <- primIO $ prim__deref_bits16 (stateAddr + 18)
      
      usedIdx <- primIO $ prim__deref_bits16 (rxUsed + 2)
      println $ "Net: pollNet idx=" ++ b64ToHexString (cast idx) ++ " usedIdx=" ++ b64ToHexString (cast usedIdx) ++ " lastUsed=" ++ b64ToHexString (cast lastUsed)
      if usedIdx > lastUsed
        then do
          pktLen32 <- primIO $ prim__deref_bits32 (rxUsed + 8)
          println $ "Net: pollNet pktLen=" ++ b64ToHexString (cast pktLen32)
          when (pktLen32 > 10) $ do
            let frameStart = rxBuf + 10
                frameLen   : Bits64 = cast pktLen32 - 10
            println $ "Net: pollNet frameLen=" ++ b64ToHexString frameLen
            when (frameLen > 14) $
              dispatchFrame stateAddr base frameStart frameLen
          primIO $ prim__set_bits16 (stateAddr + 18) usedIdx
          println $ "Net: pollNet processed usedIdx=" ++ b64ToHexString (cast usedIdx)
        else println "Net: pollNet no packet"

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
setupNetwork : {numPages : Nat} -> Bits64 -> Kernel numPages ()
setupNetwork addr = do
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

      let vq       = getHeapAddr rx_vq
          buffer   = getHeapAddr rx_buffer
          desc     = vq
          avail    = vq + 16
          rxUsed   = vq + 24
          txVq     = getHeapAddr tx_vq
          txDesc   = txVq
          txAvail  = txVq + 16
          txBuf    = getHeapAddr tx_buffer
          state    = getHeapAddr statePg

      let statusReg = addr + MMIO_VIRTIO_STATUS

      -- RESET
      primIO $ prim__set_bits32 statusReg 0
      -- ACK
      primIO $ prim__set_bits32 statusReg 1
      -- DRIVER
      primIO $ prim__set_bits32 statusReg 3

      -- Read device features
      features <- primIO $ prim__deref_bits32 (addr + 0x10)
      println $ "Net: device features=0x" ++ b64ToHexString (cast features)

      -- Accept features but clear MRG_RXBUF (bit 15) so header stays 10 bytes
      let drvFeatures = features .&. complement (1 `shiftL` 15)
      primIO $ prim__set_bits32 (addr + 0x20) drvFeatures
      -- FEATURES_OK
      primIO $ prim__set_bits32 statusReg 11

      -- ---- RX Queue (queue 0) ----
      primIO $ prim__set_bits32 (addr + 0x30) 0
      primIO $ prim__set_bits32 (addr + 0x28) 4096
      primIO $ prim__set_bits32 (addr + 0x38) 1
      -- Set QueueAlign to 4 so used ring fits in same page as descriptors
      primIO $ prim__set_bits32 (addr + 0x3c) 4

      -- RX Descriptor 0 (WRITE flag = 2, device writes to buffer)
      primIO $ prim__set_bits64 desc buffer
      primIO $ prim__set_bits32 (desc + 8) 2048
      primIO $ prim__set_bits16 (desc + 12) 2
      primIO $ prim__set_bits16 (desc + 14) 0
      println $ "Net: RX desc at 0x" ++ b64ToHexString desc ++ " buffer=0x" ++ b64ToHexString buffer

      -- RX Available ring: flags=0, idx=1, ring[0]=0
      primIO $ prim__set_bits16 avail 0
      primIO $ prim__set_bits16 (avail + 2) 1
      primIO $ prim__set_bits16 (avail + 4) 0
      println $ "Net: RX avail ring at 0x" ++ b64ToHexString avail ++ " idx=1 ring[0]=0"

      -- RX Used ring
      primIO $ prim__set_bits16 rxUsed 0
      primIO $ prim__set_bits16 (rxUsed + 2) 0

      -- QueuePFN for RX
      primIO $ prim__set_bits32 (addr + 0x40) (cast $ shiftR vq 12)

      -- ---- TX Queue (queue 1) ----
      primIO $ prim__set_bits32 (addr + 0x30) 1
      primIO $ prim__set_bits32 (addr + 0x38) 1
      primIO $ prim__set_bits32 (addr + 0x3c) 4

      -- TX Descriptor 0 (flags=0, driver writes to buffer)
      primIO $ prim__set_bits64 txDesc txBuf
      primIO $ prim__set_bits32 (txDesc + 8) 2048
      primIO $ prim__set_bits16 (txDesc + 12) 0
      primIO $ prim__set_bits16 (txDesc + 14) 0

      -- TX Available ring: idx=0
      primIO $ prim__set_bits16 txAvail 0
      primIO $ prim__set_bits16 (txAvail + 2) 0
      primIO $ prim__set_bits16 (txAvail + 4) 0

      -- QueuePFN for TX
      primIO $ prim__set_bits32 (addr + 0x40) (cast $ shiftR txVq 12)

      -- DRIVER_OK
      primIO $ prim__set_bits32 statusReg 7

      -- Notify device that RX descriptor is available
      primIO $ prim__set_bits32 (addr + 0x50) 0
      println "Net: notified device of RX descriptor"

      -- Read MAC from VirtIO config space (base + 0x100)
      mac0 <- primIO $ prim__deref_bits8 (addr + 0x100)
      mac1 <- primIO $ prim__deref_bits8 (addr + 0x101)
      mac2 <- primIO $ prim__deref_bits8 (addr + 0x102)
      mac3 <- primIO $ prim__deref_bits8 (addr + 0x103)
      mac4 <- primIO $ prim__deref_bits8 (addr + 0x104)
      mac5 <- primIO $ prim__deref_bits8 (addr + 0x105)
      println $ "Net: MAC=" ++ b64ToHexString (cast mac0) ++ ":" ++ b64ToHexString (cast mac1) ++ ":" ++ b64ToHexString (cast mac2) ++ ":" ++ b64ToHexString (cast mac3) ++ ":" ++ b64ToHexString (cast mac4) ++ ":" ++ b64ToHexString (cast mac5)

      -- Write state page
      primIO $ prim__set_bits64 state addr              -- offset 0: virtio_base
      primIO $ prim__set_bits64 (state + 8) avail       -- offset 8: rx_avail_addr
      primIO $ prim__set_bits16 (state + 16) 1          -- offset 16: rx_avail_idx (matches avail ring)
      primIO $ prim__set_bits16 (state + 18) 0          -- offset 18: last_used_idx
      primIO $ prim__set_bits64 (state + 24) rxUsed     -- offset 24: rx_used_addr
      primIO $ prim__set_bits64 (state + 32) buffer     -- offset 32: rx_buffer_addr
      primIO $ prim__set_bits64 (state + 40) txDesc     -- offset 40: tx_desc_addr
      primIO $ prim__set_bits64 (state + 48) txAvail    -- offset 48: tx_avail_addr
      primIO $ prim__set_bits64 (state + 56) txBuf      -- offset 56: tx_buffer_addr
      primIO $ prim__set_bits16 (state + 64) 0          -- offset 64: tx_avail_idx
      -- our_mac at offset 72
      primIO $ prim__set_bits8 (state + 72) mac0
      primIO $ prim__set_bits8 (state + 73) mac1
      primIO $ prim__set_bits8 (state + 74) mac2
      primIO $ prim__set_bits8 (state + 75) mac3
      primIO $ prim__set_bits8 (state + 76) mac4
      primIO $ prim__set_bits8 (state + 77) mac5
      -- our_seq at offset 80
      primIO $ prim__set_bits32 (state + 80) 0x1000
      -- rest: peer_seq_next, peer_port, peer_ip, peer_mac start as 0
      primIO $ prim__set_bits32 (state + 84) 0
      primIO $ prim__set_bits16 (state + 88) 0
      primIO $ prim__set_bits32 (state + 92) 0

      -- Set mscratch to state page address via ecall into M-mode
      primIO $ prim__make_syscall state

      println "Virtio network setup complete."
