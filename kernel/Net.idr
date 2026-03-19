module Net

import Data.Bits
import Data.C.Array8
import Data.C.Extra
import Data.Nat
import Data.String.Extra
import Heap
import Kernel
import Pages
import Prelude.Extra.Num
import Uart
import VirtIO

-- Allocate a page and identity-map it
export
allocAndMap : {numPages : Nat} -> (0 _ : LT 1 numPages) => Kernel numPages (Either AllocPagesErrors HeapAddr)
allocAndMap = do
  Right page <- liftInit $ zalloc (mkNatPos 1)
    | Left err => pure (Left err)
  Right () <- mmap (getHeapAddr page) (getHeapAddr page) entryBits.ReadWrite
    | Left err => pure (Left err)
  pure (Right page)

export
setupNetwork : {numPages : Nat} -> Bits64 -> Kernel numPages ()
setupNetwork addr = do
  case isLT 1 numPages of
    No _ => println "Page table too small for network setup"
    Yes prf => do
      -- Allocate 1 page for the virtqueue (desc + avail + used contiguous)
      -- and 1 page for the RX buffer
      Right rx_vq     <- allocAndMap @{prf}
        | Left err => println $ "Net: alloc rx_vq failed: " ++ show err
      Right rx_buffer <- allocAndMap @{prf}
        | Left err => println $ "Net: alloc rx_buffer failed: " ++ show err

      println "Setup network"

      -- Layout within the single virtqueue page (contiguous, v1 legacy):
      --   offset 0:   descriptor table (16 bytes per entry, 1 entry = 16 bytes)
      --   offset 16:  available ring (flags:2 + idx:2 + ring[1]:2 + used_event:2 = 8 bytes)
      --   offset 4096-8: used ring at end of page (flags:2 + idx:2 + elem[1]:{id:4,len:4} = 12 bytes)
      --   Actually for v1 legacy, the layout is computed from QueuePFN:
      --     desc starts at QueuePFN * page_size
      --     avail starts at desc + 16 * queue_size  (= desc + 16 for size=1)
      --     used starts at align(avail + 4 + 2 * queue_size, 4096)
      --   With queue_size=1, page_size=4096:
      --     desc  = base + 0
      --     avail = base + 16
      --     used  = align(base + 16 + 4 + 2, 4096) = base + 4096 if base is page-aligned
      --   So used ring needs to be on the NEXT page. We need 2 pages for the vq.

      Right rx_used_pg <- allocAndMap @{prf}
        | Left err => println $ "Net: alloc rx_used failed: " ++ show err

      let vq     = getHeapAddr rx_vq
          buffer = getHeapAddr rx_buffer
          desc   = vq            -- offset 0
          avail  = vq + 16       -- right after 1 descriptor
          used   = getHeapAddr rx_used_pg  -- next page-aligned address

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

      -- Skip features negotiation (v1 legacy, same as asm-os)

      ---------------------------------------------------------
      -- Select queue 0 (RX) : QUEUE_SEL @ base + 0x30
      ---------------------------------------------------------
      primIO $ prim__set_bits32 (addr + 0x30) 0

      ---------------------------------------------------------
      -- Set GuestPageSize @ base + 0x28 = 4096 (required for v1 legacy)
      ---------------------------------------------------------
      primIO $ prim__set_bits32 (addr + 0x28) 4096

      ---------------------------------------------------------
      -- Set queue size = 1 : QUEUE_NUM @ base + 0x38
      ---------------------------------------------------------
      primIO $ prim__set_bits32 (addr + 0x38) 1

      ---------------------------------------------------------
      -- Descriptor 0
      ---------------------------------------------------------
      -- desc[0].addr (8 bytes at offset 0)
      primIO $ prim__set_bits64 desc buffer
      -- desc[0].len (4 bytes at offset 8)
      primIO $ prim__set_bits32 (desc + 8) 2048
      -- desc[0].flags (2 bytes at offset 12) = VIRTQ_DESC_F_WRITE
      primIO $ prim__set_bits16 (desc + 12) 2
      -- desc[0].next (2 bytes at offset 14)
      primIO $ prim__set_bits16 (desc + 14) 0

      ---------------------------------------------------------
      -- Available ring (contiguous after desc)
      ---------------------------------------------------------
      -- flags (2 bytes)
      primIO $ prim__set_bits16 avail 0
      -- idx (2 bytes)
      primIO $ prim__set_bits16 (avail + 2) 1
      -- ring[0] (2 bytes)
      primIO $ prim__set_bits16 (avail + 4) 0

      ---------------------------------------------------------
      -- Used ring (on separate page, page-aligned)
      ---------------------------------------------------------
      -- flags (2 bytes)
      primIO $ prim__set_bits16 used 0
      -- idx (2 bytes)
      primIO $ prim__set_bits16 (used + 2) 0

      ---------------------------------------------------------
      -- QueuePFN @ base + 0x40 = physical_address / page_size
      ---------------------------------------------------------
      primIO $ prim__set_bits32 (addr + 0x40) (cast $ shiftR vq 12)

      ---------------------------------------------------------
      -- DRIVER_OK (1 | 2 | 4) = 7
      ---------------------------------------------------------
      primIO $ prim__set_bits32 statusReg 7

      println "Virtio network setup complete."
