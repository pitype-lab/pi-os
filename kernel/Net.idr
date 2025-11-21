module Net

import Constants
import Data.C.Ptr
import Lib
import Uart
import VirtIO

export
setupNetwork : Bits64 -> IO ()
setupNetwork addr = do
  println "Setup network"
  let statusReg = cast {to=AnyPtr} $ cast {to=Bits64} $ addr + MMIO_VIRTIO_STATUS 
  ------ RESET 
  setPtr statusReg $ cast {to=Bits8} 0
  ------ ACK
  setPtr statusReg $ cast {to=Bits8} 1
  ------ DRIVER
  setPtr statusReg $ cast {to=Bits8} 3
  ------ Select queue 0 (RX)
  let queue0Reg = cast {to=AnyPtr} $ addr + 0x30
  queue0 <- deref {a=Bits64} queue0Reg
  ------ Select queue = 1
  let queue1Reg = cast {to=AnyPtr} $ addr + 0x38
  queue1 <- deref {a=Bits64} queue1Reg
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
