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

-- Set mscratch via ecall (traps into M-mode, fast path in trap.s)
%foreign "C:make_syscall"
prim__make_syscall : Bits64 -> PrimIO ()

-- Read mscratch — only valid from M-mode (called from trap handler)
%foreign "C:read_mscratch"
prim__read_mscratch : PrimIO Bits64

-- State page layout:
--   offset 0:  virtio base address (Bits64)
--   offset 8:  avail ring address (Bits64)
--   offset 16: avail ring index (Bits16)

-- Handle a virtio-net interrupt: ack, recycle RX buffer, notify device
-- Called from m_trap which runs in M-mode, so read_mscratch is valid.
export
handleNetIrq : IO ()
handleNetIrq = do
  stateAddr <- primIO prim__read_mscratch
  if stateAddr == 0
    then pure ()
    else do
      base  <- primIO $ prim__deref_bits64 stateAddr
      avail <- primIO $ prim__deref_bits64 (stateAddr + 8)
      idx   <- primIO $ prim__deref_bits16 (stateAddr + 16)

      -- 1. Ack the virtio interrupt
      isr <- primIO $ prim__deref_bits32 (base + 0x60)
      primIO $ prim__set_bits32 (base + 0x64) isr

      -- 2. Recycle RX descriptor
      let newIdx = idx + 1
      primIO $ prim__set_bits16 (avail + 4) 0       -- ring[0] = descriptor 0
      primIO $ prim__set_bits16 (avail + 2) newIdx   -- bump avail idx

      -- 3. Notify device
      primIO $ prim__set_bits32 (base + 0x50) 0

      -- 4. Update index in state page
      primIO $ prim__set_bits16 (stateAddr + 16) newIdx

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
      Right rx_vq     <- allocAndMap @{prf}
        | Left err => println $ "Net: alloc rx_vq failed: " ++ show err
      Right rx_buffer <- allocAndMap @{prf}
        | Left err => println $ "Net: alloc rx_buffer failed: " ++ show err

      println "Setup network"

      Right rx_used_pg <- allocAndMap @{prf}
        | Left err => println $ "Net: alloc rx_used failed: " ++ show err

      -- Allocate a state page for the IRQ handler
      Right statePg <- allocAndMap @{prf}
        | Left err => println $ "Net: alloc state page failed: " ++ show err

      let vq     = getHeapAddr rx_vq
          buffer = getHeapAddr rx_buffer
          desc   = vq
          avail  = vq + 16
          used   = getHeapAddr rx_used_pg
          state  = getHeapAddr statePg

      let statusReg = addr + MMIO_VIRTIO_STATUS

      -- RESET
      primIO $ prim__set_bits32 statusReg 0
      -- ACK
      primIO $ prim__set_bits32 statusReg 1
      -- DRIVER
      primIO $ prim__set_bits32 statusReg 3

      -- Select queue 0
      primIO $ prim__set_bits32 (addr + 0x30) 0
      -- GuestPageSize
      primIO $ prim__set_bits32 (addr + 0x28) 4096
      -- Queue size
      primIO $ prim__set_bits32 (addr + 0x38) 1

      -- Descriptor 0
      primIO $ prim__set_bits64 desc buffer
      primIO $ prim__set_bits32 (desc + 8) 2048
      primIO $ prim__set_bits16 (desc + 12) 2
      primIO $ prim__set_bits16 (desc + 14) 0

      -- Available ring
      primIO $ prim__set_bits16 avail 0
      primIO $ prim__set_bits16 (avail + 2) 1
      primIO $ prim__set_bits16 (avail + 4) 0

      -- Used ring
      primIO $ prim__set_bits16 used 0
      primIO $ prim__set_bits16 (used + 2) 0

      -- QueuePFN
      primIO $ prim__set_bits32 (addr + 0x40) (cast $ shiftR vq 12)

      -- DRIVER_OK
      primIO $ prim__set_bits32 statusReg 7

      -- Write state page: base, avail addr, avail idx
      primIO $ prim__set_bits64 state addr
      primIO $ prim__set_bits64 (state + 8) avail
      primIO $ prim__set_bits16 (state + 16) 1

      -- Set mscratch to state page address via ecall into M-mode
      primIO $ prim__make_syscall state

      println "Virtio network setup complete."
