module Main

import Data.C.Ptr
import Data.IORef
import Pages
import Plic as Plic
import Trap
import Uart

import Data.List
import Data.Bits
import Data.Vect

%export "urefc:Main_kinit"
kinit : IO Nat
kinit = do
  println "Init PI OS memory"
  let init_pages =  replicate (cast numPages) Empty
  pagesRef <- newIORef init_pages
  page <- zalloc pagesRef ((cast {to=Nat} $ (cast {to=Double} numPages) / (cast {to=Double} pageSize))+1)
  root <- zalloc pagesRef 1
  println "Map text section"
  id_map_range pagesRef root (cast_AnyPtrNat textStart) (cast_AnyPtrNat textEnd) ReadExecute
  println "Map data section"
  id_map_range pagesRef root (cast_AnyPtrNat dataStart) (cast_AnyPtrNat dataEnd) ReadExecute
  println "Map rodata section"
  id_map_range pagesRef root (cast_AnyPtrNat rodataStart) (cast_AnyPtrNat rodataEnd) ReadExecute
  println "Map bss section"
  id_map_range pagesRef root (cast_AnyPtrNat bssStart) (cast_AnyPtrNat bssEnd) ReadWrite
  println "Map kernel stack section"
  id_map_range pagesRef root (cast_AnyPtrNat kernelStackStart) (cast_AnyPtrNat kernelStackEnd) ReadWrite
  println "Map kernel heap section"
  id_map_range pagesRef root (cast_AnyPtrNat kernelStackEnd) (cast_AnyPtrNat mallocStart) ReadWrite
  println "Map kernel uart section"
  map pagesRef root 0x10000000 0x10000100 ReadWrite
  println "Map kernel debug exit section"
  map pagesRef root 0x100000 0x100000 ReadWrite
  println "Map CLINT section"
  map pagesRef root 0x02000000 0x0200ffff ReadWrite
  map pagesRef root 0x0200b000 0x0200c000 ReadWrite
  println "Map PLIC section"
  id_map_range pagesRef root 0x0c000000 0x0c002000 ReadWrite
  id_map_range pagesRef root 0x0c200000 0x0c208000 ReadWrite
  id_map_range pagesRef root (cast_AnyPtrNat page) ((cast_AnyPtrNat page)+numPages) ReadWrite
  println "Save page"
  savePages page pagesRef
  println "Finish initialising memory"
  pure $ cast $ (shiftR (cast {to=Bits64} (cast_AnyPtrNat root)) 12) .|. (shiftL 8 60) 

main : IO ()
main = do
  println "Welcome to PI-OS!"
  pagesRef <- getPages
  println "Setting up interrupts and PLIC"
  --Plic.set_threshold 0
  --Plic.enable 10
  --Plic.set_priority 10 1
  println "Bye !"
  exit





