module VirtIO

import Constants
import Data.C.Ptr
import Data.Bits
import Lib
import Uart

MMIO_VIRTIO_START : Bits64
MMIO_VIRTIO_START = 0x10001000

MMIO_VIRTIO_END : Bits64
MMIO_VIRTIO_END = 0x10008000

MMIO_VIRTIO_STRIDE : Bits64
MMIO_VIRTIO_STRIDE =  0x1000

export
MMIO_VIRTIO_MAGIC : Bits64
MMIO_VIRTIO_MAGIC = 0x74726976

public export
MMIO_VIRTIO_STATUS : Bits64
MMIO_VIRTIO_STATUS =  0x70


public export
record InitVirtIO where
  constructor MkInitVirtIO
  initNetwork : Bits64 -> IO ()

public export
record VirtQueueAvailable where
  flags : Bits16
  idx : Bits16
  ring : Bits16
  usedEvent : Bits16


private
setup : Bits64 -> Bits64 -> InitVirtIO -> IO ()
setup 1 addr init = init.initNetwork addr
setup n _ _ = println $ "Virtio " ++ show n ++ "not implemented yet" 

export
probe : InitVirtIO -> IO ()
probe initVirtIO = do
  for_ [MMIO_VIRTIO_START, MMIO_VIRTIO_START+MMIO_VIRTIO_STRIDE..MMIO_VIRTIO_END] $ \addr => do
    magicvalue <- deref {a=Bits32} $ cast addr
    deviceid <- deref {a=Bits32} $ cast $ addr+8

    when (magicvalue /= (cast MMIO_VIRTIO_MAGIC)) $ do
      println "Not virtio"

    if deviceid == 0
      then println "Not connected"
      else setup (cast deviceid) addr initVirtIO
    
    pure ()

