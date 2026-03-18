module Pages

import Data.Bits
import Data.C.Extra
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
import Uart

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

-- MMU Entry Bits
public export
record EntryBits where
  constructor MkEntryBits
  Valid : Bits64
  Read : Bits64
  Write : Bits64
  Execute : Bits64
  User : Bits64
  Global : Bits64
  Access : Bits64
  Dirty : Bits64

  ReadWrite : Bits64
  ReadExecute : Bits64
  ReadWriteExecute : Bits64

  UserReadWrite : Bits64
  UserReadExecute : Bits64
  UserReadWriteExecute : Bits64

export
entryBits : EntryBits
entryBits = MkEntryBits {
  Valid              = shiftL 1 0,
  Read               = shiftL 1 1,
  Write              = shiftL 1 2,
  Execute            = shiftL 1 3,
  User               = shiftL 1 4,
  Global             = shiftL 1 5,
  Access             = shiftL 1 6,
  Dirty              = shiftL 1 7,

  ReadWrite          = shiftL 1 1 .|. shiftL 1 2,
  ReadExecute        = shiftL 1 1 .|. shiftL 1 3,
  ReadWriteExecute   = shiftL 1 1 .|. shiftL 1 2 .|. shiftL 1 3,

  UserReadWrite         = shiftL 1 1 .|. shiftL 1 2 .|. shiftL 1 4,
  UserReadExecute       = shiftL 1 1 .|. shiftL 1 3 .|. shiftL 1 4,
  UserReadWriteExecute  = shiftL 1 1 .|. shiftL 1 2 .|. shiftL 1 3 .|. shiftL 1 4
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
    memset1 arr numPages pageBits.Empty
    pure arr
  pure (numPages ** arr)

export
alloc : {numPages : Nat} -> (size : NatPos) -> (0 _ : LT (fst size) numPages) => Kernel numPages (Either AllocPagesErrors HeapAddr)
alloc (Element(S last) _) = do
  let size = S (last)
  pageTable <- ask
  res <- runIO $ withIArray pageTable $ \iPageTable => 
    getFirstFreeSpace @{rewrite plusZeroRightNeutral last in %search} iPageTable 0 last
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
zalloc : {numPages : Nat} -> (size : NatPos) -> (0 _ : LT (fst size) numPages) => Kernel numPages (Either AllocPagesErrors HeapAddr)
zalloc size = do
  res <- alloc size
  case res of
    Right heapAddr => do
      let byteCount = cast (fst size) * pageSize
      liftIO $ zero_heap heapAddr byteCount
      pure (Right heapAddr)
    Left err => pure (Left err)

export
dealloc : {numPages : Nat} -> HeapAddr -> Kernel numPages ()
dealloc heapAddr = do
  let addr = getHeapAddr heapAddr
      location : Nat = cast $ toDouble (addr - allocStart) / toDouble pageSize
  println $ show location
  pageTable <- ask 
  case isLT location numPages of
    Yes prf => runIO $ free pageTable (natToFinLT location)
    No _    => pure ()

  where 
    free : (pageTable : CArray8IO numPages) -> (index : Fin numPages) -> F1' World
    free pageTable i = T1.do
      page <- get pageTable i
      if page /= pageBits.Last
         then T1.do
           set pageTable i pageBits.Empty
           let next = S (finToNat i)
           case isLT next numPages of
             Yes prf => free pageTable (natToFinLT next)
             No _    => pure ()
         else
           set pageTable i pageBits.Empty

export
mmap : {numPages : Nat}
    -> (root : HeapAddr)
    -> (vaddr : Bits64)
    -> (paddr : Bits64)
    -> (bits : Bits64)
    -> Kernel numPages (Either AllocPagesErrors ())
mmap root vaddr paddr bits = do
    let vpn2 = shiftR vaddr 30 .&. 0x1ff
        vpn1 = shiftR vaddr 21 .&. 0x1ff
        vpn0 = shiftR vaddr 12 .&. 0x1ff

        ppn2 = shiftR paddr 30 .&. 0x3ffffff
        ppn1 = shiftR paddr 21 .&. 0x1ff
        ppn0 = shiftR paddr 12 .&. 0x1ff

    case mkHeapAddr (getHeapAddr root + vpn2 * 8) of
      Nothing => pure (Left HeapOutOfBounds)
      Just entryPtr2 => do
        res <- traverseLevel entryPtr2 vpn1
        case res of
          Left err => pure (Left err)
          Right entryPtr1 => do
            res <- traverseLevel entryPtr1 vpn0
            case res of
              Left err => pure (Left err)
              Right entryPtr0 => do
                let entry =
                      shiftL ppn2 28 .|.
                      shiftL ppn1 19 .|.
                      shiftL ppn0 10 .|.
                      bits .|.
                      entryBits.Valid .|.
                      entryBits.Dirty .|.
                      entryBits.Access
                write_heap_bits64 entryPtr0 entry
                pure (Right ())

  where
    traverseLevel : HeapAddr -> Bits64 -> Kernel numPages (Either AllocPagesErrors HeapAddr)
    traverseLevel entryAddr vpn = do
      val <- read_heap_bits64 entryAddr
      when ((val .&. entryBits.Valid) == 0) $ do
        case isLT 1 numPages of
          Yes prf => do
            res <- zalloc @{prf} (mkNatPos 1)
            case res of
              Right page =>
                write_heap_bits64 entryAddr (shiftR (getHeapAddr page) 2 .|. entryBits.Valid)
              Left _ => pure ()
          No _ => pure ()
      val <- liftIO $ read_heap_bits64 entryAddr
      if (val .&. entryBits.Valid) == 0
        then pure (Left NoMemory)
        else do
          let nextTable = shiftL (val .&. complement 0x3ff) 2
          case mkHeapAddr (nextTable + vpn * 8) of
            Nothing => pure (Left HeapOutOfBounds)
            Just addr => pure (Right addr)

export
idMapRange : {numPages : Nat}
          -> (root : HeapAddr)
          -> (start : Bits64)
          -> (end : Bits64)
          -> (bits : Bits64)
          -> Kernel numPages (Either AllocPagesErrors ())
idMapRange root start end bits = do
    let memaddr = start .&. complement (pageSize - 1)
        alignEnd = (end + pageSize - 1) .&. complement (pageSize - 1)
        numKbPages = cast {to=Nat} $ toDouble (alignEnd - memaddr) / toDouble pageSize
    mapAddr numKbPages memaddr

  where
    mapAddr : Nat -> Bits64 -> Kernel numPages (Either AllocPagesErrors ())
    mapAddr Z _ = pure (Right ())
    mapAddr (S n) addr = do
      res <- mmap root addr addr bits
      case res of
        Left err => pure (Left err)
        Right () => mapAddr n (addr + pageSize)


