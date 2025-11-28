module Plic

import Data.Bits
import Data.C.Ptr
import Data.C.Ptr.Extra

PLIC_PTR_PRIORITY : AnyPtr
PLIC_PTR_PRIORITY = cast {to=AnyPtr} $ the Bits64 0x0c000000

PLIC_PTR_PENDING : AnyPtr
PLIC_PTR_PENDING = cast {to=AnyPtr} $ the Bits64 0x0c001000

PLIC_PTR_ENABLE : AnyPtr
PLIC_PTR_ENABLE = cast {to=AnyPtr} $ the Bits64 0x0c002000

PLIC_PTR_THRESHOLD : AnyPtr
PLIC_PTR_THRESHOLD  = cast {to=AnyPtr} $ the Bits64 0x0c200000

PLIC_PTR_CLAIM : AnyPtr
PLIC_PTR_CLAIM  = cast {to=AnyPtr} $ the Bits64 0x0c200004

export
set_threshold : Nat -> IO ()
set_threshold tsh = do
  let actual_prio = (cast {to=Bits32} tsh) .&. 7
  setPtr PLIC_PTR_THRESHOLD actual_prio

export
enable : Nat -> IO ()
enable id = do
  let actual_id = cast $ shiftL 1 id
  enables <- deref {a=Bits32} PLIC_PTR_ENABLE
  setPtr PLIC_PTR_ENABLE (enables .|. actual_id)

export
set_priority : Nat -> Nat -> IO ()
set_priority id prio = do
  let actual_prio = (cast {to=Bits32} prio) .&. 7
  setPtr (prim__inc_ptr PLIC_PTR_PRIORITY (cast id) 1) actual_prio
