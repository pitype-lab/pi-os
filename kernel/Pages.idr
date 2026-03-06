module Pages

import Data.Bits
import Data.IORef
import Data.List
import Heap
import Prelude.Extra.Num
import Uart
import Data.So

export
pageSize: Bits64
pageSize = 1 `shiftL` 12 

pageOrder : Nat
pageOrder = 12

export
numPages : Nat
numPages = cast $ floor $ toDouble heapSize / toDouble pageSize

export
data PageBits = Empty | Taken | Last

export
Show PageBits where
  show Empty = "Empty"
  show Taken = "Taken"
  show Last = "Last"

-- Ensure that mkPages is the only way to create Pages, so that we can guarantee the length of the list of PageBits is always numPages
namespace PagesCon
  export
  data Pages : Type where
    MkPages : 
         (xs : List PageBits)
      -> (0 _ : So (length xs == numPages))
      -> Pages

  export
  mkPages : List PageBits -> Maybe Pages
  mkPages xs = 
    case decSo $ length xs == numPages of
      Yes prf => Just (MkPages xs prf)
      No _    => Nothing

  export
  getPages : Pages -> List PageBits
  getPages (MkPages xs prf) = xs

pages : Maybe Pages
pages = mkPages (replicate numPages Empty)

data AllocPagesErrors = NoMemory | HeapOutOfBounds

alignVal : Bits64 -> Nat -> Bits64
alignVal val order = 
  let o = cast $ 1  `shiftL` order
  in (val + o) .&. complement o


-- TODO currently returning a heap address. It would be better to return the heap address + the number of pages allocated
export
alloc : IORef Pages -> NatPos -> IO (Either AllocPagesErrors HeapAddr)
alloc ref (size ** prf) = do
  pages <- readIORef ref >>= (pure . getPages)
  case getFirstFreeSpace pages [] 0 of
    Nothing => pure $ Left NoMemory
    Just (pages, location) => 
      case mkHeapAddr $ alignVal (heapStart + cast location * pageSize) pageOrder of
        Just addr =>  pure $ Right addr
        Nothing => pure $ Left HeapOutOfBounds

  where
    isFree : List PageBits -> Nat -> Bool
    isFree (Empty::xs) Z = True
    isFree (Empty::xs) (S n) = isFree xs n
    isFree _ _= False

    getFirstFreeSpace :
         (pages : List PageBits)
      -> (res : List PageBits) 
      -> (location : Nat) 
      -> Maybe (List PageBits, Nat)
    getFirstFreeSpace [] _ _ = Nothing
    getFirstFreeSpace (x::xs) res location = 
      if isFree (x::xs) size
         then Just (reverse res ++ replicate size Taken ++ Last::drop size xs, location)
         else getFirstFreeSpace xs (x::res) (location+1)








