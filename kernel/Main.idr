module Main

import Control.App
import Data.Bits
import Data.C.Array8
import Data.List
import Data.IORef
import Data.String.Extra
import Heap
import Memory
import Pages
import Prelude.Extra.Num
import System
import Uart

kinit : Has [HasPages, HasUart] e => App e ()
kinit = do
  putStrLn "Init PI OS memory"
  result <- alloc (mkNatPos 4)
  pure ()

%export "urefc:Main_runKInit"
runKInit : IO ()
runKInit = do
  pageTable <- runIO (calloc1 numPages) <&> \pageTable => (numPages ** pageTable)
  run $ env {tag = PageTableTag} pageTable kinit

main : IO ()
main = pure ()

