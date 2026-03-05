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
write_mmio_bits8 : MMIO -> Bits8 -> IO ()
write_mmio_bits8 UART val = primIO $ prim__set_bits8 virtIO_MMIO.UART val

export
write_mmio_bits16 : MMIO -> Bits16 -> IO ()
write_mmio_bits16 UART val = primIO $ prim__set_bits16 virtIO_MMIO.UART val

export
write_mmio_bits32 : MMIO -> Bits32 -> IO ()
write_mmio_bits32 UART val = primIO $ prim__set_bits32 virtIO_MMIO.UART val

export
write_mmio_bits64 : MMIO -> Bits64 -> IO ()
write_mmio_bits64 UART val = primIO $ prim__set_bits64 virtIO_MMIO.UART val



