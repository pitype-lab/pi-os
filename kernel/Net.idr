module Net

import Data.Bits
import Data.C.Array8
import Data.C.Extra
import Data.Nat
import Heap
import Kernel
import Pages
import Prelude.Extra.Num
import Uart
import VirtIO

-- Allocate a page and identity-map it
allocAndMap : {numPages : Nat} -> (0 _ : LT 1 numPages) => HeapAddr -> Kernel numPages (Either AllocPagesErrors HeapAddr)
allocAndMap root = do
  Right page <- zalloc (mkNatPos 1)
    | Left err => pure (Left err)
  Right () <- mmap root (getHeapAddr page) (getHeapAddr page) entryBits.ReadWrite
    | Left err => pure (Left err)
  pure (Right page)

export
setupNetwork : {numPages : Nat} -> HeapAddr -> Bits64 -> Kernel numPages ()
setupNetwork root addr = do
  case isLT 1 numPages of
    No _ => println "Page table too small for network setup"
    Yes prf => do
      Right rx_desc   <- allocAndMap @{prf} root
        | Left err => println $ "Net: alloc rx_desc failed: " ++ show err
      Right rx_avail  <- allocAndMap @{prf} root
        | Left err => println $ "Net: alloc rx_avail failed: " ++ show err
      Right rx_used   <- allocAndMap @{prf} root
        | Left err => println $ "Net: alloc rx_used failed: " ++ show err
      Right rx_buffer <- allocAndMap @{prf} root
        | Left err => println $ "Net: alloc rx_buffer failed: " ++ show err

      println "Setup network"

      let desc   = getHeapAddr rx_desc
          avail  = getHeapAddr rx_avail
          used   = getHeapAddr rx_used
          buffer = getHeapAddr rx_buffer

      ---------------------------------------------------------
      -- STATUS register @ base + 0x70
      ---------------------------------------------------------
      let statusReg = addr + MMIO_VIRTIO_STATUS

      -- RESET
      primIO $ prim__set_bits32 statusReg 0

      -- ACK (1)
      primIO $ prim__set_bits32 statusReg 1

      -- DRIVER (1 | 2) = 3
      primIO $ prim__set_bits32 statusReg 3

      ---------------------------------------------------------
      -- Select queue 0 (RX) : QUEUE_SEL @ base + 0x30
      ---------------------------------------------------------
      primIO $ prim__set_bits32 (addr + 0x30) 0

      ---------------------------------------------------------
      -- Set queue size = 1 : QUEUE_NUM @ base + 0x38
      ---------------------------------------------------------
      primIO $ prim__set_bits32 (addr + 0x38) 1

      ---------------------------------------------------------
      -- Descriptor 0 at rx_desc
      ---------------------------------------------------------

      -- desc[0].addr (8 bytes at offset 0)
      primIO $ prim__set_bits64 desc buffer

      -- desc[0].len (4 bytes at offset 8)
      primIO $ prim__set_bits32 (desc + 8) 2048

      -- desc[0].flags (2 bytes at offset 12)
      primIO $ prim__set_bits16 (desc + 12) 2   -- VIRTQ_DESC_F_WRITE

      -- desc[0].next (2 bytes at offset 14)
      primIO $ prim__set_bits16 (desc + 14) 0

      ---------------------------------------------------------
      -- Setup avail ring (rx_avail)
      ---------------------------------------------------------

      -- flags (2 bytes at offset 0)
      primIO $ prim__set_bits16 avail 0

      -- idx (2 bytes at offset 2)
      primIO $ prim__set_bits16 (avail + 2) 1

      -- ring[0] (2 bytes at offset 4)
      primIO $ prim__set_bits16 (avail + 4) 0

      ---------------------------------------------------------
      -- Setup used ring (rx_used)
      ---------------------------------------------------------

      -- flags (4 bytes at offset 0)
      primIO $ prim__set_bits32 used 0

      -- idx (4 bytes at offset 4)
      primIO $ prim__set_bits32 (used + 4) 0

      ---------------------------------------------------------
      -- Queue physical addresses
      ---------------------------------------------------------

      -- QUEUE_DESC_LOW @ base + 0x80
      primIO $ prim__set_bits32 (addr + 0x80) (cast desc)

      -- QUEUE_DESC_HIGH @ base + 0x84
      primIO $ prim__set_bits32 (addr + 0x84) (cast $ shiftR desc 32)

      -- QUEUE_AVAIL_LOW @ base + 0x90
      primIO $ prim__set_bits32 (addr + 0x90) (cast avail)

      -- QUEUE_AVAIL_HIGH @ base + 0x94
      primIO $ prim__set_bits32 (addr + 0x94) (cast $ shiftR avail 32)

      -- QUEUE_USED_LOW @ base + 0xA0
      primIO $ prim__set_bits32 (addr + 0xA0) (cast used)

      -- QUEUE_USED_HIGH @ base + 0xA4
      primIO $ prim__set_bits32 (addr + 0xA4) (cast $ shiftR used 32)

      -- QUEUE_READY @ base + 0x44
      primIO $ prim__set_bits32 (addr + 0x44) 1

      ---------------------------------------------------------
      -- DRIVER_OK -> 1 | 2 | 4 = 7
      ---------------------------------------------------------
      primIO $ prim__set_bits32 statusReg 7

      println "Virtio network setup complete."
