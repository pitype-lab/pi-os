module Pages

import Data.C.Ptr
import Data.Bits
import Data.List
import Data.IORef
import Data.Vect

import Debug
import Uart
import Trap

public export
data PageBits = Empty | Taken | Last

export
Show PageBits where
  show Empty = "Empty"
  show Taken = "Taken"
  show Last = "Last"

export
pageSize: Nat
pageSize = 4096

%foreign "C:idris2_anyptr_nat"
prim_cast_anyptr_nat : AnyPtr -> Nat

export
cast_AnyPtrNat: AnyPtr -> Nat 
cast_AnyPtrNat = prim_cast_anyptr_nat

%foreign "C:idris2_heap_size"
prim__idris2_heap_size: AnyPtr 

export
heapSize : Nat 
heapSize = cast_AnyPtrNat prim__idris2_heap_size

%foreign "C:idris2_heap_start"
prim__idris2_heap_start: AnyPtr

export
heapStart : AnyPtr
heapStart = prim__idris2_heap_start

export
numPages : Nat
numPages = cast {to=Nat} $ (cast {to=Double} heapSize) / (cast {to=Double} pageSize)

export
alloc : IORef (List PageBits) -> Nat -> IO AnyPtr
alloc ref Z = do
  println "Cannot allocate size of 0"
  exit
  pure heapStart
alloc ref (S size) = do
  pages <- readIORef ref
  case getFirstFreeSpace pages [] 0 of
       Nothing => do
        println "No memory available"
        exit
        pure heapStart
       Just (pages, location) => do
         writeIORef ref pages
         pure $ prim__inc_ptr heapStart (cast pageSize) (cast location)

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

export
dealloc : IORef (List PageBits) -> AnyPtr -> IO ()
dealloc ref ptr = do
  let heapStartAddr = cast_AnyPtrNat heapStart
  let ptrAddr = cast_AnyPtrNat ptr
  let pageNum = cast {to=Nat} $ ((cast {to=Double} ptrAddr) - (cast {to=Double} heapStartAddr)) / (cast {to=Double} pageSize)
  println $ "NumPage : " ++ show pageNum
  pages <- readIORef ref
  free (drop pageNum pages) [] >>= \p => writeIORef ref (take pageNum pages ++ p)

  where
    free : List PageBits -> List PageBits -> IO (List PageBits)
    free [] _ = do
      println "Couldn't free"
      exit
      pure []
    free (Last::ps) res = pure $ res ++ (Empty::ps) 
    free (p::ps) rest = free ps (Empty::rest)

export
zalloc : IORef (List PageBits) -> Nat -> IO AnyPtr
zalloc ref size = do
  ptr <- alloc ref size
  zeroPages ptr size
  pure ptr

  where
    zero : AnyPtr -> Nat -> IO ()
    zero ptr Z = setPtr ptr $ cast {to=Bits8} 0
    zero ptr (S n) = do
      setPtr (prim__inc_ptr heapStart (cast n) 1) $ cast {to=Bits8} 0
      zero ptr n

    zeroPages : AnyPtr -> Nat -> IO ()
    zeroPages ptr Z = zero ptr pageSize
    zeroPages ptr (S n) = do
      zero ptr pageSize
      zeroPages ptr n

savePages : IORef (List PageBits) -> IO ()
savePages ref = do
  pages <- readIORef ref
  save pages 0

  where 
    save : List PageBits -> Bits32 -> IO ()
    save [] n = pure ()
    save (Empty::xs) n = do
      setPtr (prim__inc_ptr heapStart n  1) $ cast {to=Bits8} 0
      save xs (n+1)
    save (Taken::xs) n = do
      setPtr (prim__inc_ptr heapStart (cast n) 1) $ cast {to=Bits8} 1
      save xs (n+1)
    save (Last::xs) n =  do
      setPtr (prim__inc_ptr heapStart (cast n) 1) $ cast {to=Bits8} 2
      save xs (n+1)

export
getPages :  IO (IORef (List PageBits))
getPages = do
  pages <- read numPages
  newIORef (reverse pages)

  where
    read : Nat -> IO (List PageBits)
    read Z = pure []
    read (S n) = do
      let ptr = (prim__inc_ptr heapStart (cast n) 1)
      val <- deref {a=Bits8} ptr
      case val of
        1 => do
          xs <- read n
          pure (Taken::xs)
        2 => do
          xs <- read n
          pure (Last::xs)
        _ => do
          xs <- read n
          pure (Empty::xs)

data EntryBits = 
    None 
  | Valid 
  | Read 
  | Write 
  | Execute 
  | User 
  | Global
  | Access
  | Dirty

  -- Convenience combinations
  | ReadWrite
  | ReadExecute
  | ReadWriteExecute

  -- User Convenience Combinations
  | UserReadWrite
  | UserReadExecute
  | UserReadWriteExecute

implementation Cast EntryBits Bits64 where
  cast None = 0
  cast Valid = shiftL 1 0
  cast Read = shiftL 1 1
  cast Write = shiftL 1 2
  cast Execute = shiftL 1 3
  cast User = shiftL 1 4
  cast Global = shiftL 1 5
  cast Access = shiftL 1 6
  cast Dirty = shiftL 1 7

  -- Convenience combinations
  cast ReadWrite = (shiftL 1 1) .|. (shiftL 1 2)
  cast ReadExecute = (shiftL 1 1) .|. (shiftL 1 3)
  cast ReadWriteExecute = (shiftL 1 1) .|. (shiftL 1 2) .|. (shiftL 1 3) 

  -- Convenience combinations
  cast UserReadWrite = (shiftL 1 1) .|. (shiftL 1 2) .|. (shiftL 1 4) 
  cast UserReadExecute = (shiftL 1 1) .|. (shiftL 1 3) .|. (shiftL 1 4) 
  cast UserReadWriteExecute = (shiftL 1 1) .|. (shiftL 1 2) .|. (shiftL 1 3) .|. (shiftL 1 4) 

map : 
     IORef (List PageBits)
  -> (root : AnyPtr)
  -> (vaddr : Nat) 
  -> (paddr : Nat)
  -> (bits: EntryBits)
  -> IO ()
map pagesRef root vaddr paddr bits = do
    let vpn : Vect 3 Bits64 = [
          shiftR (cast vaddr) 12 .&. 0x1ff,
          shiftR (cast vaddr) 21 .&. 0x1ff,
          shiftR (cast vaddr) 30 .&. 0x1ff]
    let ppn : Vect 3 Bits64 = [
          shiftR (cast paddr) 12 .&. 0x1ff,
          shiftR (cast paddr) 21 .&. 0x1ff,
          shiftR (cast paddr) 30 .&. 0x3ffffff]
    let v =  (prim__inc_ptr root (cast $ (index 2 vpn) * 8) 1)
    leaf <- traversePageTable vpn 0 v
    let entry =
      shiftL (index 2 ppn) 28 .|.
      shiftL (index 1 ppn) 19 .|.
      shiftL (index 0 ppn) 10 .|. (cast {to=Bits64} bits) .|. (cast {to=Bits64} Valid)
    setPtr leaf entry

    where
      traversePageTable : (vpn : Vect 3 Bits64) -> (level : Fin 3) ->  (v : AnyPtr) -> IO AnyPtr
      traversePageTable vpn n v =
        if n >= 0 && n < 2
           then do
               val <- deref {a=Bits64} v
               if val /= (cast {to=Bits64} Valid)
                then do
                  page <- zalloc pagesRef 1
                  let n = cast_AnyPtrNat page
                  setPtr v $ cast {to=Bits64} ((shiftR (cast {to=Bits64} (cast_AnyPtrNat page)) 2) .|. (cast {to=Bits64} Valid))
                else pure ()
               entry <- deref {a=Bits64} v >>= \val => pure $ (val .&. (complement 0x3ff)) `shiftL` 2
               let v = (prim__inc_ptr v (cast $ (index n vpn) * 8) 1)
               if n == 1
                  then traversePageTable vpn 0 v
                  else pure v
           else pure v

export
testPages : IO ()
testPages = do
  println "Test pages"
  let init_pages =  replicate (cast numPages) Empty
  pagesRef <- newIORef init_pages
  root <- zalloc pagesRef 1
  map pagesRef root 0x10000000 0x10000000 ReadWrite









