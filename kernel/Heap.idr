module Heap

import Data.C.Extra
import Data.So
import public Memory

-- Ensure that mkHeapAddr is the only way to create a heap address, so that we can guarantee that all heap addresses are within the bounds of the heap
namespace HeapCon
  export
  data HeapAddr : Type where
    MkHeapAddr : (addr : Bits64) 
      -> (0 _ : So (addr >= heapStart && addr <= heapStart + heapSize)) 
      -> HeapAddr

  export
  mkHeapAddr : Bits64 -> Maybe HeapAddr
  mkHeapAddr addr = 
    case decSo (addr >= heapStart && addr <= heapStart + heapSize) of
      Yes prf   => Just (MkHeapAddr addr prf)
      No  _     => Nothing

  export
  getHeapAddr : HeapAddr -> Bits64
  getHeapAddr (MkHeapAddr addr prf) = addr

export
read_heap_bits64 : HasIO io => HeapAddr -> io Bits64
read_heap_bits64 addr = primIO $ prim__deref_bits64 (getHeapAddr addr)

export
write_heap_bits8 : HasIO io => HeapAddr -> Bits8 -> io ()
write_heap_bits8 addr val = primIO $ prim__set_bits8 (getHeapAddr addr) val

export
write_heap_bits16 : HasIO io => HeapAddr -> Bits16 -> io ()
write_heap_bits16 addr val = primIO $ prim__set_bits16 (getHeapAddr addr) val

export
write_heap_bits32 : HasIO io => HeapAddr -> Bits32 -> io ()
write_heap_bits32 addr val = primIO $ prim__set_bits32 (getHeapAddr addr) val

export
write_heap_bits64 : HasIO io => HeapAddr -> Bits64 -> io ()
write_heap_bits64 addr val = primIO $ prim__set_bits64 (getHeapAddr addr) val

export
increment_heap_addr_bits8 : HeapAddr -> Maybe HeapAddr
increment_heap_addr_bits8 addr = 
  let newAddr = getHeapAddr addr + 1 in
  mkHeapAddr newAddr

export
increment_heap_addr_bits16 : HeapAddr -> Maybe HeapAddr
increment_heap_addr_bits16 addr = 
  let newAddr = getHeapAddr addr + 2 in
  mkHeapAddr newAddr

export
increment_heap_addr_bits32 : HeapAddr -> Maybe HeapAddr
increment_heap_addr_bits32 addr = 
  let newAddr = getHeapAddr addr + 4 in
  mkHeapAddr newAddr

export
increment_heap_addr_bits64 : HeapAddr -> Maybe HeapAddr
increment_heap_addr_bits64 addr = 
  let newAddr = getHeapAddr addr + 8 in
  mkHeapAddr newAddr

export
zero_heap : HasIO io => HeapAddr -> Bits64 -> io ()
zero_heap addr n = do
  let _ = prim__memset (getHeapAddr addr) 0 n
  pure ()
