module MMIO

public export
data MMIO = UART

record VirtIO_MMIO where
  constructor MkVirtIOMMIO
  UART : Bits64

virtIO_MMIO = MkVirtIOMMIO {
  UART = 0x10000000
}

%foreign "C:write_mmio_bits8"
prim_write_mmio_bits8: Bits64 -> Bits8 -> IO ()

%foreign "C:write_mmio_bits16"
prim_write_mmio_bits16: Bits64 -> Bits16 -> IO ()

%foreign "C:write_mmio_bits32"
prim_write_mmio_bits32: Bits64 -> Bits32 -> IO ()

%foreign "C:write_mmio_bits64"
prim_write_mmio_bits64: Bits64 -> Bits64 -> IO ()

export
write_mmio_bits8 : MMIO -> Bits8 -> IO ()
write_mmio_bits8 UART val = prim_write_mmio_bits8 virtIO_MMIO.UART val

export
write_mmio_bits16 : MMIO -> Bits16 -> IO ()
write_mmio_bits16 UART val = prim_write_mmio_bits16 virtIO_MMIO.UART val

export
write_mmio_bits32 : MMIO -> Bits32 -> IO ()
write_mmio_bits32 UART val = prim_write_mmio_bits32 virtIO_MMIO.UART val

export
write_mmio_bits64 : MMIO -> Bits64 -> IO ()
write_mmio_bits64 UART val = prim_write_mmio_bits64 virtIO_MMIO.UART val



