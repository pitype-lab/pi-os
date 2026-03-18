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

kinit : {n : Nat} -> Kernel n ()
kinit = do
  println "Init PI OS memory"
  case isLT 4 n of
    Yes prf => do
      res <- alloc @{prf} (mkNatPos 4)
      pure ()
    No _ => println "Page table too small"
  case isLT 2 n of
    Yes prf => do
      res <- alloc @{prf} (mkNatPos 2)
      pure ()
    No _ => println "Page table too small"
  pageTable <- ask
  case isLTE 10 n of
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

