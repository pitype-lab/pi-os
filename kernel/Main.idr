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
  println "Map text section"
  id_map_range pagesRef root (cast_AnyPtrNat textStart) (cast_AnyPtrNat textEnd) ReadWrite
  println "Map data section"
  id_map_range pagesRef root (cast_AnyPtrNat dataStart) (cast_AnyPtrNat dataEnd) ReadWrite
  println "Map rodata section"
  id_map_range pagesRef root (cast_AnyPtrNat rodataStart) (cast_AnyPtrNat rodataEnd) ReadWrite
  println "Map bss section"
  id_map_range pagesRef root (cast_AnyPtrNat bssStart) (cast_AnyPtrNat bssEnd) ReadWrite
  println "Map kernel stack section"
  id_map_range pagesRef root (cast_AnyPtrNat kernelStackStart) (cast_AnyPtrNat kernelStackEnd) ReadWrite
  println "Map kernel heap section"
  id_map_range pagesRef root (cast_AnyPtrNat kernelStackEnd) (cast_AnyPtrNat mallocStart) ReadWrite
  println "Map kernel uart section"
  map pagesRef root 0x10000000 0x10000000 ReadWrite
  println "Finish initialising memory"
  pure $ cast $ (shiftR (cast {to=Bits64} (cast_AnyPtrNat root)) 12) .|. (shiftL 60 8)

main : IO ()
main = do
  println "Welcome to PI-OS!"
  println "Bye !"
  exit





