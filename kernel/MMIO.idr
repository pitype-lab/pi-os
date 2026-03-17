module MMIO

import Data.C.Extra

public export
data MMIO = UART

record VirtIO_MMIO where
  constructor MkVirtIOMMIO
  UART : Bits64

virtIO_MMIO = MkVirtIOMMIO {
  UART = 0x10000000
}

export
write_mmio_bits8 : HasIO io => MMIO -> Bits8 -> io ()
write_mmio_bits8 UART val = primIO $ prim__set_bits8 virtIO_MMIO.UART val

export
write_mmio_bits16 : HasIO io => MMIO -> Bits16 -> io ()
write_mmio_bits16 UART val = primIO $ prim__set_bits16 virtIO_MMIO.UART val

export
write_mmio_bits32 : HasIO io => MMIO -> Bits32 -> io ()
write_mmio_bits32 UART val = primIO $ prim__set_bits32 virtIO_MMIO.UART val

export
write_mmio_bits64 : HasIO io => MMIO -> Bits64 -> io ()
write_mmio_bits64 UART val = primIO $ prim__set_bits64 virtIO_MMIO.UART val



