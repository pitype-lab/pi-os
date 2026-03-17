module Main

import Data.Bits
import Data.C.Array8
import Data.List
import Data.String.Extra
import Heap
import Kernel
import Memory
import Pages
import Prelude.Extra.Num
import System
import Uart

kinit : Kernel ()
kinit = do
  println "Init PI OS memory"
  result <- alloc (mkNatPos 4)
  pure ()

%export "urefc:Main_runKInit"
runKInit : IO ()
runKInit = do
  pageTable <- runIO (malloc1 numPages) <&> \pt => (numPages ** pt)
  runReaderT pageTable kinit
  exit

main : IO ()
main = pure ()

