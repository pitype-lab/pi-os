module Net

import Data.C.Ptr
import Data.C.Ptr.Extra
import Data.IORef
import Pages
import Uart
import VirtIO

export
setupNetwork : Bits64 -> IORef (List PageBits) -> IO ()
setupNetwork addr pagesRef = do
  println "Setup network"

  rx_desc   <- alloc pagesRef 1
  rx_avail  <- alloc pagesRef 1
  rx_used   <- alloc pagesRef 1
  rx_buffer <- alloc pagesRef 1

  ---------------------------------------------------------
  -- MMIO base pointer
  ---------------------------------------------------------
  let basePtr : AnyPtr = cast addr

  ---------------------------------------------------------
  -- STATUS register @ 0x70
  ---------------------------------------------------------
  let statusReg : Ptr Bits32 = cast $ prim__inc_ptr basePtr (sizeof Bits8) MMIO_VIRTIO_STATUS

  -- RESET
  setPtr statusReg (the Bits32 0)

  -- ACK  (1)
  setPtr statusReg (the Bits32 1)

  -- DRIVER (1 | 2) = 3
  setPtr statusReg (the Bits32 3)

  ---------------------------------------------------------
  -- Select queue 0 (RX) : QUEUE_SEL @ 0x30
  ---------------------------------------------------------
  let queueSel : Ptr Bits32 = cast $ prim__inc_ptr basePtr (sizeof Bits8)  0x30
  setPtr queueSel (the Bits32 0)

  ---------------------------------------------------------
  -- Set queue size = 1 : QUEUE_NUM @ 0x38
  ---------------------------------------------------------
  let queueNum : Ptr Bits32 = cast $ prim__inc_ptr basePtr (sizeof Bits8)  0x38
  setPtr queueNum (the Bits32 1)

  ---------------------------------------------------------
  -- Descriptor 0 at rx_desc
  ---------------------------------------------------------

  -- desc[0].addr (8 bytes)
  let descAddr : Ptr Bits64 = cast rx_desc
  setPtr descAddr (cast {to=Bits64} rx_buffer)

  -- desc[0].len (4 bytes at offset 8)
  let descLen : Ptr Bits32 = cast $ prim__inc_ptr rx_desc (sizeof Bits8) 8
  setPtr descLen (the Bits32 2048)

  -- desc[0].flags (2 bytes at offset 12)
  let descFlags : Ptr Bits16 = cast $ prim__inc_ptr rx_desc (sizeof Bits8) 12
  setPtr descFlags (the Bits16 2)   -- VIRTQ_DESC_F_WRITE

  -- desc[0].next (2 bytes at offset 14)
  let descNext : Ptr Bits16 = cast $ prim__inc_ptr rx_desc (sizeof Bits8) 14
  setPtr descNext (the Bits16 0)

  ---------------------------------------------------------
  -- Setup avail ring (rx_avail)
  ---------------------------------------------------------
  let availFlags : Ptr Bits16 = cast rx_avail
  setPtr availFlags (the Bits16 0)

  let availIdx : Ptr Bits16 = cast $ prim__inc_ptr rx_avail (sizeof Bits8) 2
  setPtr availIdx (the Bits16 1)

  let availRing0 : Ptr Bits16 =  cast $ prim__inc_ptr rx_avail (sizeof Bits8) 4
  setPtr availRing0 (the Bits16 0)

  ---------------------------------------------------------
  -- Setup used ring (rx_used)
  ---------------------------------------------------------
  let usedFlags : Ptr Bits32 = cast rx_used
  setPtr usedFlags (the Bits32 0)

  let usedIdx : Ptr Bits32 = cast $ prim__inc_ptr rx_used (sizeof Bits8) 4
  setPtr usedIdx (the Bits32 0)

  ---------------------------------------------------------
  -- Queue physical addresses  
  -- (LOW 32 bits only — matches your assembly)
  ---------------------------------------------------------

  -- QUEUE_DESC_LOW @ 0x40
  let qDescLow : Ptr Bits32 = cast $ prim__inc_ptr basePtr (sizeof Bits8) 0x40
  setPtr qDescLow (cast rx_desc)

  -- QUEUE_DESC_HIGH @ 0x44
  let qDescHigh : Ptr Bits32 = cast $ prim__inc_ptr basePtr (sizeof Bits8) 0x44
  setPtr qDescHigh (the Bits32 0)

  -- QUEUE_AVAIL_LOW @ 0x48
  let qAvailLow : Ptr Bits32 = cast $ prim__inc_ptr basePtr (sizeof Bits8) 0x48
  setPtr qAvailLow (cast rx_avail)

  -- QUEUE_AVAIL_HIGH @ 0x4C
  let qAvailHigh : Ptr Bits32 = cast $ prim__inc_ptr basePtr (sizeof Bits8) 0x4C
  setPtr qAvailHigh (the Bits32 0)

  -- QUEUE_USED_LOW @ 0x50
  let qUsedLow : Ptr Bits32 = cast $ prim__inc_ptr basePtr (sizeof Bits8) 0x50
  setPtr qUsedLow (cast rx_used)

  -- QUEUE_USED_HIGH @ 0x54
  let qUsedHigh : Ptr Bits32 = cast $ prim__inc_ptr basePtr (sizeof Bits8) 0x54
  setPtr qUsedHigh (the Bits32 0)

  ---------------------------------------------------------
  -- DRIVER_OK  →  1 | 2 | 4 = 7
  ---------------------------------------------------------
  setPtr statusReg (the Bits32 7)

  println "Virtio network setup complete."
