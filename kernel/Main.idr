module Main

import Data.Bits
import Data.C.Array8
import Data.C.Array8.Utils
import Data.List
import Data.Nat
import Data.String.Extra
import Syntax.T1
import Heap
import Kernel
import Memory
import Pages
import Prelude.Extra.Num
import System
import Uart

kinit : {numPages : Nat} -> Kernel numPages ()
kinit = do
  println "Init PI OS memory"
  case isLT 1 numPages of
    No _ => println "Page table too small"
    Yes prf => do
      Right root <- zalloc @{prf} (mkNatPos 1)
        | Left err => println $ "Failed to allocate root page table: " ++ show err

      Right () <- idMapRange root textStart textEnd entryBits.ReadExecute
        | Left err => println $ show err

      Right () <- idMapRange root dataStart dataEnd entryBits.ReadExecute
        | Left err => println $ show err

      Right () <- idMapRange root rodataStart rodataEnd entryBits.ReadExecute
        | Left err => println $ show err

      Right () <- idMapRange root bssStart bssEnd entryBits.ReadWrite
        | Left err => println $ show err

      Right () <- idMapRange root kernelStackStart kernelStackEnd entryBits.ReadWrite
        | Left err => println $ show err

      Right () <- idMapRange root kernelStackEnd mallocStart entryBits.ReadWrite
        | Left err => println $ show err

      Right () <- mmap root 0x10000000 0x10000000 entryBits.ReadWrite
        | Left err => println $ show err

      Right () <- mmap root 0x100000 0x100000 entryBits.ReadWrite
        | Left err => println $ show err

      Right () <- mmap root 0x02000000 0x02000000 entryBits.ReadWrite
        | Left err => println $ show err
      Right () <- mmap root 0x0200b000 0x0200b000 entryBits.ReadWrite
        | Left err => println $ show err

      Right () <- idMapRange root 0x0c000000 0x0c002020 entryBits.ReadWrite
        | Left err => println $ show err
      Right () <- idMapRange root 0x0c200000 0x0c208000 entryBits.ReadWrite
        | Left err => println $ show err

      Right () <- idMapRange root 0x10001000 0x10008020 entryBits.ReadWrite
        | Left err => println $ show err

      pure ()


%export "urefc:Main_runKInit"
runKInit : IO ()
runKInit = do
  (n ** pageTable) <- initPageTable
  runKernel pageTable kinit
  exit

main : IO ()
main = pure ()

