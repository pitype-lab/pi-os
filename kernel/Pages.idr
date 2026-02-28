module Pages

import Data.C.Ptr
import Data.C.Ptr.Extra
import Data.Bits
import Data.List
import Data.IORef
import Data.Vect
import Prelude.Extra.Num

import Debug
import Uart
import Trap

%foreign "C:idris2_heap_start"
heapStart: Nat

%foreign "C:idris2_heap_size"
heapSize: Nat

export
pageSize: Nat
pageSize = 4096

export
numPages : Nat
numPages = heapSize / pageSize

public export
data PageBits = Empty | Taken | Last

export
Show PageBits where
  show Empty = "Empty"
  show Taken = "Taken"
  show Last = "Last"








