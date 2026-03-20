module VirtIO

import Data.Bits
import Data.C.Extra
import MMIO
import Uart

MMIO_VIRTIO_STRIDE : Bits64
MMIO_VIRTIO_STRIDE = 0x1000

export
MMIO_VIRTIO_MAGIC : Bits32
MMIO_VIRTIO_MAGIC = 0x74726976

public export
record InitVirtIO where
  constructor MkInitVirtIO
  initNetwork : VirtIODevice -> IO ()

private
setup : Bits32 -> VirtIODevice -> InitVirtIO -> IO ()
setup 1 dev init = init.initNetwork dev
setup n _ _      = println $ "Virtio " ++ show n ++ " not implemented yet"

private
probeAddrs : Bits64 -> InitVirtIO -> IO ()
probeAddrs addr initVirtIO =
  if addr > VIRTIO_MMIO_END
    then pure ()
    else do
      case mkVirtIODevice addr of
        Nothing  => println $ "VirtIO: address out of range"
        Just dev => do
          magicvalue <- readVirtIO dev MagicValue
          deviceid   <- readVirtIO dev DeviceID

          when (magicvalue /= MMIO_VIRTIO_MAGIC) $
            println "Not virtio"

          if deviceid == 0
            then pure ()
            else setup deviceid dev initVirtIO

      probeAddrs (addr + MMIO_VIRTIO_STRIDE) initVirtIO

export
probe : InitVirtIO -> IO ()
probe = probeAddrs VIRTIO_MMIO_START
