module VirtIO

import Data.Bits
import Data.C.Array8
import Data.C.Extra
import Heap
import Kernel
import Pages
import Uart

MMIO_VIRTIO_START : Bits64
MMIO_VIRTIO_START = 0x10001000

MMIO_VIRTIO_END : Bits64
MMIO_VIRTIO_END = 0x10008000

MMIO_VIRTIO_STRIDE : Bits64
MMIO_VIRTIO_STRIDE = 0x1000

export
MMIO_VIRTIO_MAGIC : Bits32
MMIO_VIRTIO_MAGIC = 0x74726976

public export
MMIO_VIRTIO_STATUS : Bits64
MMIO_VIRTIO_STATUS = 0x70

public export
record InitVirtIO (numPages : Nat) where
  constructor MkInitVirtIO
  initNetwork : Bits64 -> Kernel numPages ()

private
setup : {numPages : Nat} -> Bits32 -> Bits64 -> InitVirtIO numPages -> Kernel numPages ()
setup 1 addr init = init.initNetwork addr
setup n _ _       = println $ "Virtio " ++ show n ++ " not implemented yet"

private
probeAddrs : {numPages : Nat} -> Bits64 -> InitVirtIO numPages -> Kernel numPages ()
probeAddrs addr initVirtIO =
  if addr > MMIO_VIRTIO_END
    then pure ()
    else do
      magicvalue <- primIO $ prim__deref_bits32 addr
      deviceid   <- primIO $ prim__deref_bits32 (addr + 8)

      when (magicvalue /= MMIO_VIRTIO_MAGIC) $
        println "Not virtio"

      if deviceid == 0
        then println "Not connected"
        else setup deviceid addr initVirtIO

      probeAddrs (addr + MMIO_VIRTIO_STRIDE) initVirtIO

export
probe : {numPages : Nat} -> InitVirtIO numPages -> Kernel numPages ()
probe = probeAddrs MMIO_VIRTIO_START
