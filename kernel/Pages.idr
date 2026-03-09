module Pages

import Data.Bits
import Data.C.Ptr
import Data.C.Array8
import Data.Linear.ELift1
import public Data.IORef
import Data.List
import Heap
import Prelude.Extra.Num
import Uart
import Data.So
import public Data.Linear.Token
import Control.App

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
data PagesStateTag : Type where

public export
record PagesState where
  constructor MkPagesState
  pagesState : (n ** CArray8IO n)

data AllocPagesErrors = NoMemory | HeapOutOfBounds

export
Show AllocPagesErrors where
  show NoMemory = "Empty"
  show HeapOutOfBounds = "HeapOutOfBounds"

public export
interface HasPages e where
  alloc : NatPos -> App e (Either AllocPagesErrors HeapAddr)
  free  : HeapAddr -> App e ()


export
Has [State PagesStateTag PagesState, PrimIO] e => HasPages e where
  alloc size = do
    st <- get PagesStateTag
    let (n ** pagesState) = st.pagesState

    let i = 5
    case isLTE (S i) n of
      Yes prf => primIO $ lift1 $ setNat pagesState i pageBits.Taken
      No _ => pure () 
    
    pure (Left NoMemory)

    where
      isFree : (dpair : (n ** CIArray8 n)) -> (location : Fin (fst dpair)) -> (size : Nat) -> Bool
      isFree (n ** pagesState) location Z = at pagesState location == pageBits.Empty
      isFree (n ** pagesState) location size = False
  
  free addr = pure ()

















