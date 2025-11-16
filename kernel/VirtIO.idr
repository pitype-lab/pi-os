module VirtIO

import Data.C.Ptr
import Uart
import Data.Bits
import Lib

MMIO_VIRTIO_START : Bits32
MMIO_VIRTIO_START = 0x10001000

MMIO_VIRTIO_END : Bits32
MMIO_VIRTIO_END = 0x10008000

MMIO_VIRTIO_STATUS : Bits32
MMIO_VIRTIO_STATUS =  0x70

MMIO_VIRTIO_STRIDE : Bits32
MMIO_VIRTIO_STRIDE =  0x1000

MMIO_VIRTIO_MAGIC : Bits32
MMIO_VIRTIO_MAGIC = 0x74726976

setupNetwork : IO ()
setupNetwork = do
  ------ STATUS register address = base + 0x70
  ------ RESET 
  ------ ACK
  ------ DRIVER
  ------ Select queue 0 (RX)
  ------ Select queue = 1
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

setup : Bits32 -> IO ()
setup 1 = do
  println "Setup network"
  setupNetwork

setup n = println $ "Virtio " ++ show n ++ "not implemented yet" 

export
probe : IO ()
probe = do
  for_ [MMIO_VIRTIO_START, MMIO_VIRTIO_START+MMIO_VIRTIO_STRIDE..MMIO_VIRTIO_END] $ \addr => do
    magicvalue <- deref {a=Bits32} (cast_Bits64AnyPtr $ cast addr)
    deviceid <- deref {a=Bits32} (cast_Bits64AnyPtr $ cast addr+8)

    when (magicvalue /= MMIO_VIRTIO_MAGIC) $ do
      println "Not virtio"

    if deviceid == 0
      then println "Not connected"
      else setup deviceid
    
    pure ()

