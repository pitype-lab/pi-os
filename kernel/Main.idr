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
  case isLT 4 numPages of
    Yes prf => do
      (Right heapAddr) <- zalloc @{prf} (mkNatPos 4)
        | Left err => pure ()
      dealloc heapAddr
      pure ()
    No _ => println "Page table too small"
  case isLT 2 numPages of
    Yes prf => do
      (Right heapAddr) <- zalloc @{prf} (mkNatPos 2)
          | Left err => pure ()
      dealloc heapAddr
      pure ()
    No _ => println "Page table too small"
  pageTable <- ask
  case isLTE 10 numPages of
    Yes p => do
      iArr <- runIO $ withIArray pageTable id
      liftIO $ traverseWithIndex_ {f = IO} iArr 10 $ \i, v =>
        println (show (finToNat i) ++ ": " ++ show v)
    No _  => println "Page table has fewer than 10 entries"


%export "urefc:Main_runKInit"
runKInit : IO ()
runKInit = do
  (n ** pageTable) <- initPageTable
  runKernel pageTable kinit
  exit

main : IO ()
main = pure ()

