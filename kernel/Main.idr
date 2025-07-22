module Main

import Data.C.Ptr
import Data.IORef
import Pages
import Trap
import Uart

import Data.List
import Data.Bits

%export "urefc:Main_kinit"
kinit : IO Nat
kinit = do
  testPages
  pure 5

main : IO ()
main = do
  println "Welcome to PI-OS!"
  pagesRef <- getPages
  pages <- readIORef pagesRef
  println $ show $ length pages
  println $ show $ take 10 pages
  println "Bye !"
  exit





