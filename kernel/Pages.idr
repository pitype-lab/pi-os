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

export
cast_Bits64AnyPtr: Bits64 -> AnyPtr
cast_Bits64AnyPtr = believe_me

%foreign "C:idris2_text_start"
prim__idris2_text_start: AnyPtr 

%foreign "C:idris2_text_end"
prim__idris2_text_end: AnyPtr 

%foreign "C:idris2_data_start"
prim__idris2_data_start: AnyPtr 

%foreign "C:idris2_data_end"
prim__idris2_data_end: AnyPtr 

%foreign "C:idris2_rodata_start"
prim__idris2_rodata_start: AnyPtr 

%foreign "C:idris2_rodata_end"
prim__idris2_rodata_end: AnyPtr 

%foreign "C:idris2_bss_start"
prim__idris2_bss_start: AnyPtr 

%foreign "C:idris2_bss_end"
prim__idris2_bss_end: AnyPtr 

%foreign "C:idris2_kernel_stack_start"
prim__idris2_kernel_stack_start: AnyPtr

%foreign "C:idris2_kernel_stack_end"
prim__idris2_kernel_stack_end: AnyPtr 

%foreign "C:idris2_malloc_start"
prim__idris2_malloc_start: AnyPtr 

%foreign "C:idris2_heap_start"
prim__idris2_heap_start: AnyPtr

%foreign "C:idris2_heap_size"
prim__idris2_heap_size: AnyPtr 

export
textStart : AnyPtr
textStart = prim__idris2_text_start

export
textEnd : AnyPtr
textEnd = prim__idris2_text_end

export
dataStart : AnyPtr
dataStart = prim__idris2_data_start

export
dataEnd : AnyPtr
dataEnd = prim__idris2_data_end

export
rodataStart : AnyPtr
rodataStart = prim__idris2_rodata_start

export
rodataEnd : AnyPtr
rodataEnd = prim__idris2_rodata_end

export
bssStart : AnyPtr
bssStart = prim__idris2_bss_start

export
bssEnd : AnyPtr
bssEnd = prim__idris2_bss_end

export
kernelStackStart : AnyPtr
kernelStackStart= prim__idris2_kernel_stack_start

export
kernelStackEnd : AnyPtr
kernelStackEnd = prim__idris2_kernel_stack_end

export
mallocStart : AnyPtr
mallocStart = prim__idris2_malloc_start

export
heapStart : AnyPtr
heapStart = prim__idris2_heap_start

export
heapSize : Nat 
heapSize = cast_AnyPtrNat prim__idris2_heap_size

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
         let align = ((cast {to=Bits64} (cast_AnyPtrNat heapStart)) + ((shiftL 1 12) - 1)) .&. (complement ((shiftL 1 12) - 1))
         pure $ prim__inc_ptr (cast_Bits64AnyPtr align) (cast pageSize) (cast location)

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
  zeroPages ptr $ cast ((cast {to=Double} (size*pageSize))/8)
  pure ptr

  where
    zeroPages : AnyPtr -> Nat -> IO ()
    zeroPages ptr Z = setPtr ptr $ cast {to=Bits64} 0
    zeroPages ptr (S n) = do
       setPtr ptr $ cast {to=Bits64} 0
       zeroPages (prim__inc_ptr ptr (cast 8) 1) n

export
savePages : AnyPtr -> IORef (List PageBits) -> IO ()
savePages root ref = do
  pages <- readIORef ref
  save pages 0

  where 
    save : List PageBits -> Bits32 -> IO ()
    save [] n = pure ()
    save (Empty::xs) n = do
      setPtr (prim__inc_ptr root n  1) $ cast {to=Bits8} 0
      save xs (n+1)
    save (Taken::xs) n = do
      setPtr (prim__inc_ptr root (cast n) 1) $ cast {to=Bits8} 1
      save xs (n+1)
    save (Last::xs) n =  do
      setPtr (prim__inc_ptr root (cast n) 1) $ cast {to=Bits8} 2
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
      let align = ((cast {to=Bits64} (cast_AnyPtrNat heapStart)) + ((shiftL 1 12) - 1)) .&. (complement ((shiftL 1 12) - 1))
      let ptr = (prim__inc_ptr (cast_Bits64AnyPtr align) (cast n) 1)
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

public export
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

export
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
    val <- deref {a=Bits64} v
    leaf <- traversePageTable vpn 1 v
    let entry : Bits64 =
      shiftL (index 2 ppn) 28 .|.
      shiftL (index 1 ppn) 19 .|.
      shiftL (index 0 ppn) 10 .|. 
      cast bits .|. 
      cast Valid .|.
      cast Dirty
    setPtr leaf entry

    where
      traversePageTable : (vpn : Vect 3 Bits64) -> (level : Fin 3) ->  (v : AnyPtr) -> IO AnyPtr
      traversePageTable vpn level v = do
          val <- deref {a=Bits64} v
          when ((val .&. (cast {to=Bits64} Valid)) == 0) $ do
            page <- zalloc pagesRef 1
            setPtr v $ cast {to=Bits64} ((shiftR (cast {to=Bits64} (cast_AnyPtrNat page)) 2) .|. (cast {to=Bits64} Valid))
          val <- deref {a=Bits64} v
          let nextTableAddr = shiftL (val .&. complement 0x3ff) 2
          let nextTablePtr = cast_Bits64AnyPtr nextTableAddr
          let entryPtr = prim__inc_ptr nextTablePtr (cast $ (index level vpn) * 8) 1

          if level > 0
            then traversePageTable vpn (level-1) entryPtr
            else pure entryPtr

export
virt_to_phys : AnyPtr -> (vaddr : Nat) -> IO (Maybe Bits64)
virt_to_phys root vaddr = do
  let vpn : Vect 3 Bits64 = [
        shiftR (cast vaddr) 12 .&. 0x1ff,  -- VPN[0]
        shiftR (cast vaddr) 21 .&. 0x1ff,  -- VPN[1]
        shiftR (cast vaddr) 30 .&. 0x1ff   -- VPN[2]
      ]
  let pageOffset = cast vaddr .&. 0xfff  -- Lower 12 bits

  -- Level 2 (root level)
  entryPtr2 <- pure $ prim__inc_ptr root (cast (index 2 vpn * 8)) 1
  entry2 <- deref {a=Bits64} entryPtr2
  if entry2 .&. cast Valid == 0 then pure Nothing else do
    let tableAddr1 = shiftL (entry2 .&. complement 0x3ff) 2
    let tablePtr1 = cast_Bits64AnyPtr tableAddr1

    -- Level 1
    entryPtr1 <- pure $ prim__inc_ptr tablePtr1 (cast (index 1 vpn * 8)) 1
    entry1 <- deref {a=Bits64} entryPtr1
    if entry1 .&. cast Valid == 0 then pure Nothing else do
      let tableAddr0 = shiftL (entry1 .&. complement 0x3ff) 2
      let tablePtr0 = cast_Bits64AnyPtr tableAddr0

      -- Level 0
      entryPtr0 <- pure $ prim__inc_ptr tablePtr0 (cast (index 0 vpn * 8)) 1
      entry0 <- deref {a=Bits64} entryPtr0
      if entry0 .&. cast Valid == 0 then pure Nothing else do
        -- Check if it's a leaf (has RWX bits set)
        if (entry0 .&. 0xe) == 0 then pure Nothing else do
          let ppn0 = (entry0 `shiftR` 10) .&. 0x1ff
          let ppn1 = (entry0 `shiftR` 19) .&. 0x1ff
          let ppn2 = (entry0 `shiftR` 28) .&. 0x3ffffff
          let paddr =
                shiftL ppn2 30 .|.
                shiftL ppn1 21 .|.
                shiftL ppn0 12 .|.
                pageOffset
          pure (Just paddr)

export
id_map_range : 
     IORef (List PageBits)
  -> (root : AnyPtr)
  -> (start : Nat) 
  -> (end : Nat)
  -> (bits: EntryBits)
  -> IO ()
id_map_range pagesRef root start end bits = do
  let memaddr = (cast {to=Bits64} start) .&. (complement ((cast {to=Bits64} pageSize)-1))
  let alignEnd = ((cast {to=Bits64} end) + ((shiftL 1 12) - 1)) .&. (complement ((shiftL 1 12) - 1)) 
  let numKbPages = cast {to=Nat} $ (cast {to=Double} $ (cast {to=Bits64} alignEnd) - memaddr) / (cast {to=Double} pageSize)
  map_addr numKbPages (cast {to=Nat} memaddr)

  where 
    map_addr : (numKbPages : Nat) -> (memaddr : Nat) -> IO ()
    map_addr Z _ = pure ()
    map_addr (S numKbPages) memaddr =
      map pagesRef root memaddr memaddr bits >> map_addr numKbPages (memaddr + 4096)










