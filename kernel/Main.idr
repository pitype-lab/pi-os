module Main

import Data.C.Ptr
import Data.IORef
import Pages
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
  root <- zalloc pagesRef 1
  map pagesRef root (cast_AnyPtrNat root) (cast_AnyPtrNat root) ReadWrite 
  println "Map text section"
 -- id_map_range pagesRef root (cast_AnyPtrNat textStart) (cast_AnyPtrNat textEnd) ReadWriteExecute
  map pagesRef root 0x80006a86 0x80006a86 ReadWriteExecute
 {- println "Map data section"
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
  map pagesRef root 0x10000000 0x10000000 ReadWrite
  println "Map CLINT section"
  map pagesRef root 0x02000000 0x02000000 ReadWrite
  println "Map MTIMECMP section"
  map pagesRef root 0x0200b000 0x0200c000 ReadWrite
  println "Map MTIME section"
  map pagesRef root 0x0200b000 0x0200c000 ReadWrite
  println "Map PLIC section"
  id_map_range pagesRef root 0x0c000000 0x0c002000 ReadWrite
  id_map_range pagesRef root 0x0c200000 0x0c208000 ReadWrite -}

  -- Print the bits in the PTE for that entry
  let vpn : Vect 3 Bits64 = [
    shiftR (0x80006a86) 12 .&. 0x1ff,
    shiftR (0x80006a86) 21 .&. 0x1ff,
    shiftR (0x80006a86) 30 .&. 0x1ff
  ]

  let lvl2 = prim__inc_ptr root (cast (index 2 vpn * 8)) 1
  val2 <- deref {a=Bits64} lvl2
  println $ "Level 2 entry: " ++ show val2

  -- Follow down to level 1
  let addr1 = shiftL (val2 .&. complement 0x3ff) 2
  let ptr1 = cast_Bits64AnyPtr addr1
  let lvl1 = prim__inc_ptr ptr1 (cast (index 1 vpn * 8)) 1
  val1 <- deref {a=Bits64} lvl1
  println $ "Level 1 entry: " ++ show val1

  -- Down to level 0
  let addr0 = shiftL (val1 .&. complement 0x3ff) 2
  let ptr0 = cast_Bits64AnyPtr addr0
  let lvl0 = prim__inc_ptr ptr0 (cast (index 0 vpn * 8)) 1
  val0 <- deref {a=Bits64} lvl0
  println $ "Level 0 entry: " ++ show val0
  
  phy <- virt_to_phys root 0x80006a86
  println $ show phy
  println "Finish initialising memory"
  println $ show $ (shiftR (cast {to=Bits64} (cast_AnyPtrNat root)) 12) .|. (shiftL 8 60)
  pure $ cast $ (shiftR (cast {to=Bits64} (cast_AnyPtrNat root)) 12) .|. (shiftL 8 60) .|. shiftL 0 44


main : IO ()
main = do
  println "Welcome to PI-OS!"
  println "Bye !"
  exit





