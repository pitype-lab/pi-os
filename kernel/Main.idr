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

kinit : {numPages : Nat} -> Kernel numPages (Maybe HeapAddr)
kinit = do
  println "Init PI OS MMU"
  case isLT 1 numPages of
    No _ => do println "Page table too small"
               pure Nothing
    Yes prf => do
      Right root <- zalloc @{prf} (mkNatPos 1)
        | Left err => do println $ "Failed to allocate root page table: " ++ show err
                         pure Nothing

      Right () <- idMapRange root textStart textEnd entryBits.ReadExecute
        | Left err => do println $ show err; pure Nothing

      Right () <- idMapRange root dataStart dataEnd entryBits.ReadWrite
        | Left err => do println $ show err; pure Nothing

      Right () <- idMapRange root rodataStart rodataEnd entryBits.ReadExecute
        | Left err => do println $ show err; pure Nothing

      Right () <- idMapRange root bssStart bssEnd entryBits.ReadWrite
        | Left err => do println $ show err; pure Nothing

      Right () <- idMapRange root kernelStackStart kernelStackEnd entryBits.ReadWrite
        | Left err => do println $ show err; pure Nothing

      Right () <- idMapRange root kernelStackEnd mallocStart entryBits.ReadWrite
        | Left err => do println $ show err; pure Nothing

      Right () <- mmap root 0x10000000 0x10000000 entryBits.ReadWrite
        | Left err => do println $ show err; pure Nothing

      Right () <- mmap root 0x100000 0x100000 entryBits.ReadWrite
        | Left err => do println $ show err; pure Nothing

      Right () <- mmap root 0x02000000 0x02000000 entryBits.ReadWrite
        | Left err => do println $ show err; pure Nothing
      Right () <- mmap root 0x0200b000 0x0200b000 entryBits.ReadWrite
        | Left err => do println $ show err; pure Nothing

      Right () <- idMapRange root 0x0c000000 0x0c002020 entryBits.ReadWrite
        | Left err => do println $ show err; pure Nothing
      Right () <- idMapRange root 0x0c200000 0x0c208000 entryBits.ReadWrite
        | Left err => do println $ show err; pure Nothing

      Right () <- idMapRange root 0x10001000 0x10008020 entryBits.ReadWrite
        | Left err => do println $ show err; pure Nothing

      -- Map only the heap pages allocated so far (page table nodes).
      -- Allocate a marker to find the current high water mark.
      Right marker <- zalloc @{prf} (mkNatPos 1)
        | Left err => do println $ show err; pure Nothing
      Right () <- idMapRange root heapStart (getHeapAddr marker + pageSize) entryBits.ReadWrite
        | Left err => do println $ show err; pure Nothing

      enterSupervisorMode (makeSatp root)

      println "Running in S-mode"
      pure (Just root)

kmain : {numPages : Nat} -> Kernel numPages ()
kmain = do
  mroot <- kinit
  case mroot of
    Nothing => println "Failed to initialize kernel"
    Just root => do
      Plic.set_threshold 0
      Plic.enable 8
      Plic.set_priority 8 1
      probe (MkInitVirtIO (setupNetwork root))

main : IO ()
main = do
  (n ** pageTable) <- initPageTable
  runKernel pageTable kmain
