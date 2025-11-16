module VirtIO

import Data.C.Ptr
import Uart
import Data.Bits
import Net
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

