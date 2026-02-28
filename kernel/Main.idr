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
kinit : IO ()
kinit = println "Init PI OS memory"

main : IO ()
main = println "Welcome to PI-OS!"





