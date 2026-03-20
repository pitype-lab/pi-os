module MMIO

import Data.C.Extra
import Data.So
import public Heap

------------------------------------------------------------------------
-- Platform MMIO devices
------------------------------------------------------------------------

public export
data MMIO = UART
          | ClintMtimecmp | ClintMtime
          | PlicPriority | PlicPending | PlicEnable | PlicThreshold | PlicClaim

record PlatformMMIO where
  constructor MkPlatformMMIO
  UART : Bits64
  ClintMtimecmp : Bits64
  ClintMtime : Bits64
  PlicPriority : Bits64
  PlicPending : Bits64
  PlicEnable : Bits64
  PlicThreshold : Bits64
  PlicClaim : Bits64

platformMMIO : PlatformMMIO
platformMMIO = MkPlatformMMIO {
  UART          = 0x10000000,
  ClintMtimecmp = 0x02004000,
  ClintMtime    = 0x0200bff8,
  PlicPriority  = 0x0c000000,
  PlicPending   = 0x0c001000,
  PlicEnable    = 0x0c002000,
  PlicThreshold = 0x0c200000,
  PlicClaim     = 0x0c200004
}

export
mmioAddr : MMIO -> Bits64
mmioAddr UART          = platformMMIO.UART
mmioAddr ClintMtimecmp = platformMMIO.ClintMtimecmp
mmioAddr ClintMtime    = platformMMIO.ClintMtime
mmioAddr PlicPriority  = platformMMIO.PlicPriority
mmioAddr PlicPending   = platformMMIO.PlicPending
mmioAddr PlicEnable    = platformMMIO.PlicEnable
mmioAddr PlicThreshold = platformMMIO.PlicThreshold
mmioAddr PlicClaim     = platformMMIO.PlicClaim

-- Write functions

export
write_mmio_bits8 : HasIO io => MMIO -> Bits8 -> io ()
write_mmio_bits8 dev val = primIO $ prim__set_bits8 (mmioAddr dev) val

export
write_mmio_bits16 : HasIO io => MMIO -> Bits16 -> io ()
write_mmio_bits16 dev val = primIO $ prim__set_bits16 (mmioAddr dev) val

export
write_mmio_bits32 : HasIO io => MMIO -> Bits32 -> io ()
write_mmio_bits32 dev val = primIO $ prim__set_bits32 (mmioAddr dev) val

export
write_mmio_bits64 : HasIO io => MMIO -> Bits64 -> io ()
write_mmio_bits64 dev val = primIO $ prim__set_bits64 (mmioAddr dev) val

-- Read functions

export
read_mmio_bits8 : HasIO io => MMIO -> io Bits8
read_mmio_bits8 dev = primIO $ prim__deref_bits8 (mmioAddr dev)

export
read_mmio_bits16 : HasIO io => MMIO -> io Bits16
read_mmio_bits16 dev = primIO $ prim__deref_bits16 (mmioAddr dev)

export
read_mmio_bits32 : HasIO io => MMIO -> io Bits32
read_mmio_bits32 dev = primIO $ prim__deref_bits32 (mmioAddr dev)

export
read_mmio_bits64 : HasIO io => MMIO -> io Bits64
read_mmio_bits64 dev = primIO $ prim__deref_bits64 (mmioAddr dev)

------------------------------------------------------------------------
-- VirtIO device handle (bounds-proven)
------------------------------------------------------------------------

public export
VIRTIO_MMIO_START : Bits64
VIRTIO_MMIO_START = 0x10001000

public export
VIRTIO_MMIO_END : Bits64
VIRTIO_MMIO_END = 0x10008000

namespace VirtIOCon
  export
  data VirtIODevice : Type where
    MkVirtIODevice : (addr : Bits64)
      -> (0 _ : So (addr >= VIRTIO_MMIO_START && addr <= VIRTIO_MMIO_END))
      -> VirtIODevice

  export
  mkVirtIODevice : Bits64 -> Maybe VirtIODevice
  mkVirtIODevice addr =
    case decSo (addr >= VIRTIO_MMIO_START && addr <= VIRTIO_MMIO_END) of
      Yes prf => Just (MkVirtIODevice addr prf)
      No  _   => Nothing

  export
  getVirtIOAddr : VirtIODevice -> Bits64
  getVirtIOAddr (MkVirtIODevice addr _) = addr

------------------------------------------------------------------------
-- VirtIO MMIO registers (legacy interface)
------------------------------------------------------------------------

public export
data VirtIOReg : Type -> Type where
  MagicValue      : VirtIOReg Bits32   -- 0x00
  Version         : VirtIOReg Bits32   -- 0x04
  DeviceID        : VirtIOReg Bits32   -- 0x08
  DeviceFeatures  : VirtIOReg Bits32   -- 0x10
  DriverFeatures  : VirtIOReg Bits32   -- 0x20
  QueueSize       : VirtIOReg Bits32   -- 0x28
  QueueSel        : VirtIOReg Bits32   -- 0x30
  QueueNum        : VirtIOReg Bits32   -- 0x38
  QueueAlign      : VirtIOReg Bits32   -- 0x3c
  QueuePFN        : VirtIOReg Bits32   -- 0x40
  QueueNotify     : VirtIOReg Bits32   -- 0x50
  InterruptStatus : VirtIOReg Bits32   -- 0x60
  InterruptACK    : VirtIOReg Bits32   -- 0x64
  Status          : VirtIOReg Bits32   -- 0x70

virtioRegOffset : VirtIOReg a -> Bits64
virtioRegOffset MagicValue      = 0x00
virtioRegOffset Version         = 0x04
virtioRegOffset DeviceID        = 0x08
virtioRegOffset DeviceFeatures  = 0x10
virtioRegOffset DriverFeatures  = 0x20
virtioRegOffset QueueSize       = 0x28
virtioRegOffset QueueSel        = 0x30
virtioRegOffset QueueNum        = 0x38
virtioRegOffset QueueAlign      = 0x3c
virtioRegOffset QueuePFN        = 0x40
virtioRegOffset QueueNotify     = 0x50
virtioRegOffset InterruptStatus = 0x60
virtioRegOffset InterruptACK    = 0x64
virtioRegOffset Status          = 0x70

export
readVirtIO : HasIO io => VirtIODevice -> VirtIOReg Bits32 -> io Bits32
readVirtIO dev reg =
  primIO $ prim__deref_bits32 (getVirtIOAddr dev + virtioRegOffset reg)

export
writeVirtIO : HasIO io => VirtIODevice -> VirtIOReg Bits32 -> Bits32 -> io ()
writeVirtIO dev reg val =
  primIO $ prim__set_bits32 (getVirtIOAddr dev + virtioRegOffset reg) val

-- MAC address in VirtIO config space (base + 0x100..0x105)
export
readVirtIOMAC : HasIO io => VirtIODevice -> io (Bits8, Bits8, Bits8, Bits8, Bits8, Bits8)
readVirtIOMAC dev = do
  let base = getVirtIOAddr dev + 0x100
  m0 <- primIO $ prim__deref_bits8 base
  m1 <- primIO $ prim__deref_bits8 (base + 1)
  m2 <- primIO $ prim__deref_bits8 (base + 2)
  m3 <- primIO $ prim__deref_bits8 (base + 3)
  m4 <- primIO $ prim__deref_bits8 (base + 4)
  m5 <- primIO $ prim__deref_bits8 (base + 5)
  pure (m0, m1, m2, m3, m4, m5)

------------------------------------------------------------------------
-- Net state page fields
------------------------------------------------------------------------

public export
data NetField : Type -> Type where
  VirtioBase    : NetField Bits64   -- offset 0
  RxAvailAddr   : NetField Bits64   -- offset 8
  RxAvailIdx    : NetField Bits16   -- offset 16
  LastUsedIdx   : NetField Bits16   -- offset 18
  RxUsedAddr    : NetField Bits64   -- offset 24
  RxBufferAddr  : NetField Bits64   -- offset 32
  TxDescAddr    : NetField Bits64   -- offset 40
  TxAvailAddr   : NetField Bits64   -- offset 48
  TxBufferAddr  : NetField Bits64   -- offset 56
  TxAvailIdx    : NetField Bits16   -- offset 64
  OurSeq        : NetField Bits32   -- offset 80
  PeerSeqNext   : NetField Bits32   -- offset 84
  PeerPort      : NetField Bits16   -- offset 88
  PeerIp        : NetField Bits32   -- offset 92

export
netFieldOffset : NetField a -> Bits64
netFieldOffset VirtioBase    = 0
netFieldOffset RxAvailAddr   = 8
netFieldOffset RxAvailIdx    = 16
netFieldOffset LastUsedIdx   = 18
netFieldOffset RxUsedAddr    = 24
netFieldOffset RxBufferAddr  = 32
netFieldOffset TxDescAddr    = 40
netFieldOffset TxAvailAddr   = 48
netFieldOffset TxBufferAddr  = 56
netFieldOffset TxAvailIdx    = 64
netFieldOffset OurSeq        = 80
netFieldOffset PeerSeqNext   = 84
netFieldOffset PeerPort      = 88
netFieldOffset PeerIp        = 92

-- Read a net state field (from a HeapAddr-bounded state page)
export
readNetField : HasIO io => HeapAddr -> NetField Bits64 -> io Bits64
readNetField st fld =
  primIO $ prim__deref_bits64 (getHeapAddr st + netFieldOffset fld)

export
readNetField32 : HasIO io => HeapAddr -> NetField Bits32 -> io Bits32
readNetField32 st fld =
  primIO $ prim__deref_bits32 (getHeapAddr st + netFieldOffset fld)

export
readNetField16 : HasIO io => HeapAddr -> NetField Bits16 -> io Bits16
readNetField16 st fld =
  primIO $ prim__deref_bits16 (getHeapAddr st + netFieldOffset fld)

-- Write a net state field
export
writeNetField : HasIO io => HeapAddr -> NetField Bits64 -> Bits64 -> io ()
writeNetField st fld val =
  primIO $ prim__set_bits64 (getHeapAddr st + netFieldOffset fld) val

export
writeNetField32 : HasIO io => HeapAddr -> NetField Bits32 -> Bits32 -> io ()
writeNetField32 st fld val =
  primIO $ prim__set_bits32 (getHeapAddr st + netFieldOffset fld) val

export
writeNetField16 : HasIO io => HeapAddr -> NetField Bits16 -> Bits16 -> io ()
writeNetField16 st fld val =
  primIO $ prim__set_bits16 (getHeapAddr st + netFieldOffset fld) val

-- MAC fields: OurMac at offset 72, PeerMac at offset 96
-- These are 6-byte fields accessed via copyMac or individual byte writes

public export
data NetMacField = OurMac | PeerMac

export
netMacOffset : NetMacField -> Bits64
netMacOffset OurMac  = 72
netMacOffset PeerMac = 96

export
netMacAddr : HeapAddr -> NetMacField -> Bits64
netMacAddr st fld = getHeapAddr st + netMacOffset fld

export
writeNetMAC : HasIO io => HeapAddr -> NetMacField -> Bits8 -> Bits8 -> Bits8 -> Bits8 -> Bits8 -> Bits8 -> io ()
writeNetMAC st fld m0 m1 m2 m3 m4 m5 = do
  let base = getHeapAddr st + netMacOffset fld
  primIO $ prim__set_bits8 base       m0
  primIO $ prim__set_bits8 (base + 1) m1
  primIO $ prim__set_bits8 (base + 2) m2
  primIO $ prim__set_bits8 (base + 3) m3
  primIO $ prim__set_bits8 (base + 4) m4
  primIO $ prim__set_bits8 (base + 5) m5
