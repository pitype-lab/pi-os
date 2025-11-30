module Main

import Data.C.Ptr
import Data.C.Ptr.Extra
import Data.IORef
import Pages
import Plic as Plic
import Trap
import Uart
import Net
import VirtIO

import Data.List
import Data.Bits
import Data.Vect

%foreign "C:idris2_text_start"
textStart: Bits64

%foreign "C:idris2_text_end"
textEnd: Bits64 

%foreign "C:idris2_data_start"
dataStart: Bits64 

%foreign "C:idris2_data_end"
dataEnd: Bits64 

%foreign "C:idris2_rodata_start"
rodataStart: Bits64 

%foreign "C:idris2_rodata_end"
rodataEnd: Bits64 

%foreign "C:idris2_bss_start"
bssStart: Bits64 

%foreign "C:idris2_bss_end"
bssEnd: Bits64 

%foreign "C:idris2_kernel_stack_start"
kernelStackStart: Bits64

%foreign "C:idris2_kernel_stack_end"
kernelStackEnd: Bits64 

%foreign "C:idris2_malloc_start"
mallocStart: Bits64 

%foreign "C:idris2_heap_start"
heapStart: Bits64

%foreign "C:idris2_heap_size"
heapSize: Bits64


%export "urefc:Main_kinit"
kinit : IO Nat
kinit = do
  println "Init PI OS memory"
  let init_pages =  replicate (cast numPages) Empty
  pagesRef <- newIORef init_pages
  page <- zalloc pagesRef ((cast {to=Nat} $ (cast {to=Double} numPages) / (cast {to=Double} pageSize))+1)
  root <- zalloc pagesRef 1
  println "Map text section"
  id_map_range pagesRef root (cast textStart) (cast textEnd) ReadExecute
  println "Map data section"
  id_map_range pagesRef root (cast dataStart) (cast dataEnd) ReadExecute
  println "Map rodata section"
  id_map_range pagesRef root (cast rodataStart) (cast rodataEnd) ReadExecute
  println "Map bss section"
  id_map_range pagesRef root (cast bssStart) (cast bssEnd) ReadWrite
  println "Map kernel stack section"
  id_map_range pagesRef root (cast kernelStackStart) (cast kernelStackEnd) ReadWrite
  println "Map kernel heap section"
  id_map_range pagesRef root (cast kernelStackEnd) (cast mallocStart) ReadWrite
  println "Map kernel uart section"
  map pagesRef root 0x10000000 0x10000100 ReadWrite
  println "Map kernel debug exit section"
  map pagesRef root 0x100000 0x100000 ReadWrite
  println "Map CLINT section"
  map pagesRef root 0x02000000 0x0200ffff ReadWrite
  map pagesRef root 0x0200b000 0x0200c000 ReadWrite
  println "Map PLIC section"
  id_map_range pagesRef root 0x0c000000 0x0c002020 ReadWrite
  id_map_range pagesRef root 0x0c200000 0x0c208000 ReadWrite
  id_map_range pagesRef root (cast $ cast {to=Bits64} page) ((cast $ cast {to=Bits64} page)+(cast {to=Nat} numPages)) ReadWrite
  id_map_range pagesRef root 0x10001000 0x10008020 ReadWrite

  -- Hack to set Net virtio without kernel mem yet
  -- TODO : Remove when we will have kernel mem
  id_map_range pagesRef root (cast $ cast {to=Bits64} page+numPages) ((cast $ cast {to=Bits64} page)+(cast {to=Nat} numPages*5)) ReadWrite

  println "Save page"
  savePages page pagesRef
  println "Finish initialising memory"
  pure $ cast $ (shiftR (cast {to=Bits64} root) 12) .|. (shiftL 8 60)

main : IO ()
main = do
  println "Welcome to PI-OS!"
  pagesRef <- getPages
  println "Setting up interrupts and PLIC"
  Plic.set_threshold 0
  Plic.enable 8
  Plic.set_priority 8 1
  println "Probe virtio"
  probe pagesRef (MkInitVirtIO setupNetwork)





