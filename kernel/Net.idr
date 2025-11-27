module Net

import Data.C.Ptr
import Data.C.Ptr.Extra
import Uart
import VirtIO

export
setupNetwork : Bits64 -> IO ()
setupNetwork addr = do
  println "Setup network"
  let ptr : Ptr Bits8 = cast addr
  let statusReg = incPtr ptr MMIO_VIRTIO_STATUS 
  ------ RESET 
  setPtr statusReg $ the Bits8 0
  ------ ACK
  setPtr statusReg $ the Bits8 1
  ------ DRIVER
  setPtr statusReg $ the Bits8 3
  ------ Select queue 0 (RX)
  let ptr : AnyPtr = cast addr
  let queue0Reg : AnyPtr = prim__inc_ptr ptr (sizeof Bits8) 0x30
  queue0 : Bits32 <- deref queue0Reg
  ------ Select queue = 1
  let queue1Reg : AnyPtr = prim__inc_ptr ptr (sizeof Bits8) 0x38
  queue1 : Bits32 <- deref queue1Reg
  ------ Fill descriptor 0: addr, len, flags
  ------ For receive descriptors, the device must be allowed to write into buffer:
  ------ Set flags = VIRTQ_DESC_F_WRITE (value = 2)

  ------ Setup avail ring: flags=0, idx=1, ring[0]=0 ---

  ------ Setup used ring initial zeros
  ------ Write queue physical addresses into MMIO
  ------ QUEUE_DESC_LOW  @ 0x40
  ------ QUEUE_AVAIL_LOW
  ------ QUEUE_USED_LOW
  ------ Finally set DRIVER_OK

  pure ()
