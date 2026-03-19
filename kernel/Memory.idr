module Memory

%foreign "C:idris2_text_start"
export textStart: Bits64

%foreign "C:idris2_text_end"
export textEnd: Bits64 

%foreign "C:idris2_data_start"
export dataStart: Bits64 

%foreign "C:idris2_data_end"
export dataEnd: Bits64 

%foreign "C:idris2_rodata_start"
export rodataStart: Bits64 

%foreign "C:idris2_rodata_end"
export rodataEnd: Bits64 

%foreign "C:idris2_bss_start"
export bssStart: Bits64 

%foreign "C:idris2_bss_end"
export bssEnd: Bits64 

%foreign "C:idris2_kernel_stack_start"
export kernelStackStart: Bits64

%foreign "C:idris2_kernel_stack_end"
export kernelStackEnd: Bits64 

%foreign "C:idris2_malloc_start"
export mallocStart: Bits64 

%foreign "C:idris2_heap_start"
export heapStart: Bits64

%foreign "C:idris2_heap_size"
export heapSize: Bits64
