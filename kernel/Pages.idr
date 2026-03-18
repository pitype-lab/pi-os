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

allocStart : Bits64
allocStart = alignVal heapStart pageOrder

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
initPageTable : IO (numPages ** CArray8IO numPages)
initPageTable = do
  arr <- runIO $ T1.do
    arr <- malloc1 numPages
    fillAll arr pageBits.Empty
    pure arr
  pure (numPages ** arr)

export
alloc : {numPages : Nat} -> (size : NatPos) -> (0 _ : LT (fst size) numPages) => Kernel numPages (Either AllocPagesErrors HeapAddr)
alloc (Element(S last) _) = do
  let size = S (last)
  pageTable <- ask
  res <- runIO $ withIArray pageTable $ \iPageTable => 
    case isLT (size + 0) numPages of
      Yes prf => getFirstFreeSpace @{prf} iPageTable 0 size
      No _    => Nothing
  case res of
    Just location => 
      case isLT (last + location) numPages of
        Yes prfK => do
          let addr = allocStart + cast location * pageSize
          case mkHeapAddr addr of
               Just heapAddr => do
                 runIO $ T1.do
                   markTaken @{prfK} pageTable location last
                   setNat pageTable (last + location) pageBits.Last
                 pure (Right heapAddr)
               Nothing => pure (Left HeapOutOfBounds)
        No _ => pure (Left HeapOutOfBounds)
    Nothing => pure (Left NoMemory)

  where 
    isFree : (pageTable : CIArray8 numPages) 
          -> (location : Nat) 
          -> (size : Nat) 
          -> (0 _ : LT (size + location) numPages) 
          => Bool
    isFree pageTable location Z = 
      atNat pageTable location == pageBits.Empty
    isFree @{prf} pageTable location (S k) =
      if atNat pageTable (S k + location) == pageBits.Empty
        then isFree @{lteSuccLeft prf} pageTable location k
        else False

    markTaken : (pageTable : CArray8IO numPages)
          -> (location : Nat)
          -> (size : Nat)
          -> (0 _ : LT (size + location) numPages)
          => F1' World
    markTaken pageTable location Z = 
      setNat pageTable location pageBits.Taken
    markTaken @{prf} pageTable location (S k) = T1.do
      setNat pageTable (S k + location) pageBits.Taken
      markTaken @{lteSuccLeft prf} pageTable location k

    getFirstFreeSpace : (pageTable : CIArray8 numPages) 
          -> (location : Nat) 
          -> (size : Nat) 
          -> (0 _ : LT (size + location) numPages) 
          => Maybe Nat
    getFirstFreeSpace pageTable location size =
      if isFree pageTable location size
        then Just location
        else case isLT (size + S location) numPages of
               Yes prf => getFirstFreeSpace @{prf} pageTable (S location) size
               No _    => Nothing


export
free : Nat -> Kernel n ()
free addr = pure ()

