module Pages

import Data.Bits
import Data.IORef
import Data.List
import Heap
import Prelude.Extra.Num
import Uart

export
pageSize: Bits64
pageSize = 1 `shiftL` 12 

export
numPages : Bits64
numPages = cast $ floor $ toDouble heapSize / toDouble pageSize

export
data PageBits = Empty | Taken | Last

export
Show PageBits where
  show Empty = "Empty"
  show Taken = "Taken"
  show Last = "Last"

export
alloc : IORef (List PageBits) -> NatPos -> IO (Either String Bits64)
alloc ref (size ** prf) = do
  pages <- readIORef ref
  case getFirstFreeSpace pages [] 0 of
    Nothing => pure $ Left "No memory available"
    Just (pages, location) => pure $ Right 1

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






