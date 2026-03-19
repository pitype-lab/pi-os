module MMIO

import Data.C.Extra

public export
data MMIO = UART
          | ClintMtimecmp | ClintMtime
          | PlicPriority | PlicPending | PlicEnable | PlicThreshold | PlicClaim

record VirtIO_MMIO where
  constructor MkVirtIOMMIO
  UART : Bits64
  ClintMtimecmp : Bits64
  ClintMtime : Bits64
  PlicPriority : Bits64
  PlicPending : Bits64
  PlicEnable : Bits64
  PlicThreshold : Bits64
  PlicClaim : Bits64

virtIO_MMIO = MkVirtIOMMIO {
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
mmioAddr UART          = virtIO_MMIO.UART
mmioAddr ClintMtimecmp = virtIO_MMIO.ClintMtimecmp
mmioAddr ClintMtime    = virtIO_MMIO.ClintMtime
mmioAddr PlicPriority  = virtIO_MMIO.PlicPriority
mmioAddr PlicPending   = virtIO_MMIO.PlicPending
mmioAddr PlicEnable    = virtIO_MMIO.PlicEnable
mmioAddr PlicThreshold = virtIO_MMIO.PlicThreshold
mmioAddr PlicClaim     = virtIO_MMIO.PlicClaim

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
