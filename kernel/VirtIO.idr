module VirtIO

import Data.C.Ptr
import Data.C.Ptr.Extra
import Data.Bits
import Data.IORef
import Pages
import Uart

MMIO_VIRTIO_START : Bits64
MMIO_VIRTIO_START = 0x10001000

MMIO_VIRTIO_END : Bits64
MMIO_VIRTIO_END = 0x10008000

MMIO_VIRTIO_STRIDE : Bits64
MMIO_VIRTIO_STRIDE =  0x1000

export
MMIO_VIRTIO_MAGIC : Bits32
MMIO_VIRTIO_MAGIC = 0x74726976

public export
MMIO_VIRTIO_STATUS : Bits32
MMIO_VIRTIO_STATUS =  0x70

public export
record InitVirtIO where
  constructor MkInitVirtIO
  initNetwork : Bits64 -> IORef (List PageBits) -> IO ()

public export
record VirtQueueAvailable where
  flags : Bits16
  idx : Bits16
  ring : Bits16
  usedEvent : Bits16

private
setup : Bits32 -> Bits64 -> IORef (List PageBits) -> InitVirtIO -> IO ()
setup 1 addr pagesRef init  = init.initNetwork addr pagesRef
setup n _ _ _ = println $ "Virtio " ++ show n ++ "not implemented yet" 

export
probe : IORef (List PageBits) -> InitVirtIO -> IO ()
probe pagesRef initVirtIO = do
  for_ [MMIO_VIRTIO_START, MMIO_VIRTIO_START+MMIO_VIRTIO_STRIDE..MMIO_VIRTIO_END] $ \addr => do
    let ptr : Ptr Bits32 = cast addr
    magicvalue <- deref ptr
    deviceid <- deref $ incPtr ptr 2

    when (magicvalue /= MMIO_VIRTIO_MAGIC) $ do
      println "Not virtio"

    if deviceid == 0
      then println "Not connected"
      else setup deviceid addr pagesRef initVirtIO
    
    pure ()

