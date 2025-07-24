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
  println "Init PI OS memory"
  let init_pages =  replicate (cast numPages) Empty
  pagesRef <- newIORef init_pages
  root <- zalloc pagesRef 1
  id_map_range pagesRef root (cast_AnyPtrNat textStart) (cast_AnyPtrNat textEnd) ReadWrite
  id_map_range pagesRef root (cast_AnyPtrNat dataStart) (cast_AnyPtrNat dataEnd) ReadWrite
  id_map_range pagesRef root (cast_AnyPtrNat rodataStart) (cast_AnyPtrNat rodataEnd) ReadWrite
  id_map_range pagesRef root (cast_AnyPtrNat bssStart) (cast_AnyPtrNat bssEnd) ReadWrite
  id_map_range pagesRef root (cast_AnyPtrNat kernelStackStart) (cast_AnyPtrNat kernelStackEnd) ReadWrite
  id_map_range pagesRef root (cast_AnyPtrNat kernelStackEnd) (cast_AnyPtrNat mallocStart) ReadWrite
  map pagesRef root 0x10000000 0x10000000 ReadWrite
  pure (cast_AnyPtrNat root)

main : IO ()
main = do
  println "Welcome to PI-OS!"
  println "Bye !"
  exit





