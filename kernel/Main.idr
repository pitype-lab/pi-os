module Main

import Data.Bits
import Data.C.Array8
import Data.C.Array8.Utils
import Data.List
import Data.Nat
import Data.String.Extra
import Heap
import Kernel
import Memory
import Net
import Pages
import Prelude.Extra.Num
import System
import Trap
import Uart
import Plic
import Syntax.T1
import VirtIO

sv39Mode : Bits64
sv39Mode = 8 `shiftL` 60

makeSatp : HeapAddr -> Bits64
makeSatp root = sv39Mode .|. shiftR (getHeapAddr root) 12

kinit : {numPages : Nat} -> Kernel numPages ()
kinit = do
  root <- getRoot
  println "Init PI OS MMU"
  case isLT 1 numPages of
    No _ => println "Page table too small"
    Yes prf => do
      Right () <- idMapRange textStart textEnd entryBits.ReadExecute
        | Left err => do println $ show err

      Right () <- idMapRange dataStart dataEnd entryBits.ReadWrite
        | Left err => do println $ show err

      Right () <- idMapRange rodataStart rodataEnd entryBits.ReadExecute
        | Left err => do println $ show err

      Right () <- idMapRange bssStart bssEnd entryBits.ReadWrite
        | Left err => do println $ show err

      Right () <- idMapRange kernelStackStart kernelStackEnd entryBits.ReadWrite
        | Left err => do println $ show err

      Right () <- idMapRange kernelStackEnd mallocStart entryBits.ReadWrite
        | Left err => do println $ show err

      Right () <- mmap 0x10000000 0x10000000 entryBits.ReadWrite
        | Left err => do println $ show err

      Right () <- mmap 0x100000 0x100000 entryBits.ReadWrite
        | Left err => do println $ show err

      Right () <- mmap 0x02000000 0x02000000 entryBits.ReadWrite
        | Left err => do println $ show err
      Right () <- mmap 0x0200b000 0x0200b000 entryBits.ReadWrite
        | Left err => do println $ show err

      Right () <- idMapRange 0x0c000000 0x0c002020 entryBits.ReadWrite
        | Left err => do println $ show err
      Right () <- idMapRange 0x0c200000 0x0c208000 entryBits.ReadWrite
        | Left err => do println $ show err

      Right () <- idMapRange 0x10001000 0x10008020 entryBits.ReadWrite
        | Left err => do println $ show err

      -- Map only the heap pages allocated so far (page table nodes).
      -- Allocate a marker to find the current high water mark.
      Right marker <- liftInit $ zalloc @{prf} (mkNatPos 1)
        | Left err => do println $ show err
      Right () <- idMapRange heapStart (getHeapAddr marker + pageSize) entryBits.ReadWrite
        | Left err => do println $ show err

      enterSupervisorMode (makeSatp root)

      println "Running in S-mode"

kmain : {numPages : Nat} -> Kernel numPages ()
kmain = do
  kinit
  Plic.set_threshold 0
  Plic.enable 8
  Plic.set_priority 8 1
  probe (MkInitVirtIO setupNetwork)

main : IO ()
main = do
  (n ** pageTable) <- initPageTable
  case isLT 1 n of
    No _ => pure ()
    Yes prf => do
      -- Phase 1: allocate root page table (InitKernel, no root needed)
      mroot <- runInitKernel pageTable (zalloc @{prf} (mkNatPos 1))
      case mroot of
        Left err => println $ "Failed to allocate root: " ++ show err
        Right root => do
          -- Phase 2: run kernel with full environment (page table + root)
          let env = MkKernelEnv pageTable root
          runKernel env kmain
