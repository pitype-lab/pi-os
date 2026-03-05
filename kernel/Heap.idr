module Heap

import Data.C.Extra
import Data.So
import public Memory

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
write_heap_bits8 : HeapAddr -> Bits8 -> IO ()
write_heap_bits8 (MkHeapAddr addr prf) val = primIO $ prim__set_bits8 addr val

export
write_heap_bits16 : HeapAddr -> Bits16 -> IO ()
write_heap_bits16 (MkHeapAddr addr prf) val = primIO $ prim__set_bits16 addr val

export
write_heap_bits32 : HeapAddr -> Bits32 -> IO ()
write_heap_bits32 (MkHeapAddr addr prf) val = primIO $ prim__set_bits32 addr val

export
write_heap_bits64 : HeapAddr -> Bits64 -> IO ()
write_heap_bits64 (MkHeapAddr addr prf) val = primIO $ prim__set_bits64 addr val

