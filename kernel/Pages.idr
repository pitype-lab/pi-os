module Pages

import Data.Bits
import Data.C.Ptr
import Data.C.Array8
import Data.C.Array8.Utils
import Data.Linear.ELift1
import Data.List
import Heap
import Kernel
import Prelude.Extra.Num
import Data.So
import public Data.Linear.Token
import Data.Nat
import Data.Array.Index
import Syntax.T1

export
pageSize: Bits64
pageSize = 1 `shiftL` 12

pageOrder : Fin 64
pageOrder = 12

export
numPages : Nat
numPages = cast $ toDouble heapSize / toDouble pageSize

alignVal : Bits64 -> Fin 64 -> Bits64
alignVal val order = 
  let o = 1  `shiftL` order
  in (val + o) .&. complement o

export
record PageBits where
  constructor MkPageBits
  Empty : Bits8
  Taken : Bits8
  Last : Bits8

pageBits : PageBits
pageBits = MkPageBits {
  Empty = 0,
  Taken = 1 `shiftL` 0,
  Last = 1 `shiftL` 1
}

public export
data AllocPagesErrors = NoMemory | HeapOutOfBounds

export
Show AllocPagesErrors where
  show NoMemory = "Empty"
  show HeapOutOfBounds = "HeapOutOfBounds"

export
initPageTable : IO PageTable
initPageTable = do
  arr <- runIO $ T1.do
    arr <- malloc1 numPages
    fillAll arr pageBits.Empty
    pure arr
  pure (numPages ** arr)

export
alloc : NatPos -> Kernel (Either AllocPagesErrors HeapAddr)
alloc size = do
  (n ** arr) <- ask
  let i = 5
  case isLTE (S i) n of
    Yes prf => runIO $ setNat arr i pageBits.Taken
    No _ => pure ()
  pure (Left NoMemory)

export
free : Nat -> Kernel ()
free addr = pure ()

