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
write_heap_bits8 : HeapAddr -> Bits8 -> IO ()
write_heap_bits8 addr val = primIO $ prim__set_bits8 (getHeapAddr addr) val

export
write_heap_bits16 : HeapAddr -> Bits16 -> IO ()
write_heap_bits16 addr val = primIO $ prim__set_bits16 (getHeapAddr addr) val

export
write_heap_bits32 : HeapAddr -> Bits32 -> IO ()
write_heap_bits32 addr val = primIO $ prim__set_bits32 (getHeapAddr addr) val

export
write_heap_bits64 : HeapAddr -> Bits64 -> IO ()
write_heap_bits64 addr val = primIO $ prim__set_bits64 (getHeapAddr addr) val

export
increment_heap_addr : HeapAddr-> Maybe HeapAddr
increment_heap_addr addr = 
  let newAddr = getHeapAddr addr + 8 in
  mkHeapAddr newAddr
