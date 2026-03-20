module Data.C.Array8.Utils

import Data.Bits
import Data.C.Array8
import Data.C.Extra
import public Data.Linear.Token
import Data.Linear.ELift1
import Data.Nat
import Data.Array.Index
import Syntax.T1

%default total

||| Fill all bytes of a mutable array with a value using C memset (single FFI call).
export
memset1 : (arr : CArray8 World n) -> (n : Nat) -> Bits8 -> F1' World
memset1 arr n v t =
  let _ = prim__memset (anyPtrToBits64 $ unsafeUnwrap arr) v (cast n)
  in () # t

||| Fill all `m` elements of a mutable array with a constant value.
export
fill : (arr : CArray8 World n) -> (m : Nat) -> (0 _ : LTE m n) => Bits8 -> F1' World
fill arr 0     _ t = () # t
fill arr (S k) v t =
  let _ # t := setNat arr k v t
   in fill arr k v t

||| Fill all elements of an array with a constant value.
export
fillAll : {n : _} -> (arr : CArray8 World n) -> Bits8 -> F1' World
fillAll arr = fill arr n

||| Fill all `m` elements of a mutable array using a function over indices.
export
genFrom : (arr : CArray8 World n) -> (m : Nat) -> (0 _ : LTE m n) => (Fin n -> Bits8) -> F1' World
genFrom arr 0     f t = () # t
genFrom arr (S k) f t =
  let _ # t := setNat arr k (f $ natToFinLT k) t
   in genFrom arr k f t

||| Fill all elements of an array using a function over indices.
export
generate : {n : _} -> (arr : CArray8 World n) -> (Fin n -> Bits8) -> F1' World
generate arr = genFrom arr n

||| Apply a function to every element of an array in place.
export
mapInPlace : (arr : CArray8 World n) -> (m : Nat) -> (0 _ : LTE m n) => (Bits8 -> Bits8) -> F1' World
mapInPlace arr 0     f t = () # t
mapInPlace arr (S k) f t =
  let v # t := getNat arr k t
      _ # t := setNat arr k (f v) t
   in mapInPlace arr k f t

||| Traverse the first m elements of an immutable array with an index-aware
||| applicative action, discarding results.
export
traverseWithIndex_ :
     {n : Nat}
  -> {auto app : Applicative f}
  -> (arr : CIArray8 n)
  -> (m : Nat)
  -> (0 _ : LTE m n)
  => (Fin n -> Bits8 -> f ())
  -> f ()
traverseWithIndex_ arr 0     f = pure ()
traverseWithIndex_ arr (S k) f =
  traverseWithIndex_ arr k f *> f (natToFinLT k) (atNat arr k)

||| Traverse the first m elements of an immutable array with an applicative
||| action, discarding results.
export
traverse_ :
     {n : Nat}
  -> {auto app : Applicative f}
  -> (arr : CIArray8 n)
  -> (m : Nat)
  -> (0 _ : LTE m n)
  => (Bits8 -> f ())
  -> f ()
traverse_ arr m f = traverseWithIndex_ arr m (const f)

||| Traverse all elements of an immutable array with an index-aware applicative action.
export
traverseWithIndexAll :
     {n : Nat}
  -> {auto app : Applicative f}
  -> (arr : CIArray8 n)
  -> (Fin n -> Bits8 -> f ())
  -> f ()
traverseWithIndexAll arr = traverseWithIndex_ arr n

||| Traverse all elements of an immutable array with an applicative action.
export
traverseAll :
     {n : Nat}
  -> {auto app : Applicative f}
  -> (arr : CIArray8 n)
  -> (Bits8 -> f ())
  -> f ()
traverseAll arr = traverse_ arr n

------------------------------------------------------------------------
-- Raw pointer helpers
------------------------------------------------------------------------

private %inline
rget : AnyPtr -> Bits64 -> Bits8
rget ptr off = prim__getbits8 ptr off

private %inline
rset : AnyPtr -> Bits64 -> Bits8 -> PrimIO ()
rset ptr off val = prim__setbits8 ptr off val

-- Big-endian implementation

private
implReadBE16 : HasIO io => AnyPtr -> Bits64 -> io Bits16
implReadBE16 ptr o = pure $ (cast (rget ptr o) `shiftL` 8) .|. cast (rget ptr (o+1))

private
implWriteBE16 : HasIO io => AnyPtr -> Bits64 -> Bits16 -> io ()
implWriteBE16 ptr o v = do
  primIO $ rset ptr o     (cast $ shiftR v 8)
  primIO $ rset ptr (o+1) (cast v)

private
implReadBE32 : HasIO io => AnyPtr -> Bits64 -> io Bits32
implReadBE32 ptr o =
  pure $ (cast (rget ptr o) `shiftL` 24) .|. (cast (rget ptr (o+1)) `shiftL` 16) .|.
         (cast (rget ptr (o+2)) `shiftL` 8) .|. cast (rget ptr (o+3))

private
implWriteBE32 : HasIO io => AnyPtr -> Bits64 -> Bits32 -> io ()
implWriteBE32 ptr o v = do
  primIO $ rset ptr o     (cast $ shiftR v 24)
  primIO $ rset ptr (o+1) (cast $ shiftR v 16)
  primIO $ rset ptr (o+2) (cast $ shiftR v 8)
  primIO $ rset ptr (o+3) (cast v)

-- Little-endian implementation

private
implReadLE16 : HasIO io => AnyPtr -> Bits64 -> io Bits16
implReadLE16 ptr o = pure $ (cast (rget ptr (o+1)) `shiftL` 8) .|. cast (rget ptr o)

private
implWriteLE16 : HasIO io => AnyPtr -> Bits64 -> Bits16 -> io ()
implWriteLE16 ptr o v = do
  primIO $ rset ptr o     (cast v)
  primIO $ rset ptr (o+1) (cast $ shiftR v 8)

private
implReadLE32 : HasIO io => AnyPtr -> Bits64 -> io Bits32
implReadLE32 ptr o =
  pure $ (cast (rget ptr (o+3)) `shiftL` 24) .|. (cast (rget ptr (o+2)) `shiftL` 16) .|.
         (cast (rget ptr (o+1)) `shiftL` 8) .|. cast (rget ptr o)

private
implWriteLE32 : HasIO io => AnyPtr -> Bits64 -> Bits32 -> io ()
implWriteLE32 ptr o v = do
  primIO $ rset ptr o     (cast v)
  primIO $ rset ptr (o+1) (cast $ shiftR v 8)
  primIO $ rset ptr (o+2) (cast $ shiftR v 16)
  primIO $ rset ptr (o+3) (cast $ shiftR v 24)

private
implReadLE64 : HasIO io => AnyPtr -> Bits64 -> io Bits64
implReadLE64 ptr o = do
  lo <- implReadLE32 ptr o
  hi <- implReadLE32 ptr (o+4)
  pure $ (cast hi `shiftL` 32) .|. cast lo

private
implWriteLE64 : HasIO io => AnyPtr -> Bits64 -> Bits64 -> io ()
implWriteLE64 ptr o v = do
  implWriteLE32 ptr o     (cast v)
  implWriteLE32 ptr (o+4) (cast $ shiftR v 32)

------------------------------------------------------------------------
-- Multi-byte accessors on CArray8 (big-endian / network byte order)
-- Offset is Nat, bounds are guaranteed by the CArray8 size parameter n
-- but not statically checked per-access (to avoid unification hangs).
------------------------------------------------------------------------

export
readBE16At : HasIO io => CArray8 World n -> Nat -> io Bits16
readBE16At arr off = implReadBE16 (unsafeUnwrap arr) (cast off)

export
writeBE16At : HasIO io => CArray8 World n -> Nat -> Bits16 -> io ()
writeBE16At arr off = implWriteBE16 (unsafeUnwrap arr) (cast off)

export
readBE32At : HasIO io => CArray8 World n -> Nat -> io Bits32
readBE32At arr off = implReadBE32 (unsafeUnwrap arr) (cast off)

export
writeBE32At : HasIO io => CArray8 World n -> Nat -> Bits32 -> io ()
writeBE32At arr off = implWriteBE32 (unsafeUnwrap arr) (cast off)

------------------------------------------------------------------------
-- Multi-byte accessors on CArray8 (little-endian / native RISC-V)
------------------------------------------------------------------------

export
readLE16At : HasIO io => CArray8 World n -> Nat -> io Bits16
readLE16At arr off = implReadLE16 (unsafeUnwrap arr) (cast off)

export
writeLE16At : HasIO io => CArray8 World n -> Nat -> Bits16 -> io ()
writeLE16At arr off = implWriteLE16 (unsafeUnwrap arr) (cast off)

export
readLE32At : HasIO io => CArray8 World n -> Nat -> io Bits32
readLE32At arr off = implReadLE32 (unsafeUnwrap arr) (cast off)

export
writeLE32At : HasIO io => CArray8 World n -> Nat -> Bits32 -> io ()
writeLE32At arr off = implWriteLE32 (unsafeUnwrap arr) (cast off)

export
readLE64At : HasIO io => CArray8 World n -> Nat -> io Bits64
readLE64At arr off = implReadLE64 (unsafeUnwrap arr) (cast off)

export
writeLE64At : HasIO io => CArray8 World n -> Nat -> Bits64 -> io ()
writeLE64At arr off = implWriteLE64 (unsafeUnwrap arr) (cast off)

------------------------------------------------------------------------
-- Byte-level accessors
------------------------------------------------------------------------

export
readByteAt : HasIO io => CArray8 World n -> Nat -> io Bits8
readByteAt arr off = pure $ rget (unsafeUnwrap arr) (cast off)

export
writeByteAt : HasIO io => CArray8 World n -> Nat -> Bits8 -> io ()
writeByteAt arr off val = primIO $ rset (unsafeUnwrap arr) (cast off) val

------------------------------------------------------------------------
-- Byte-level utility operations
------------------------------------------------------------------------

export covering
copyBytesAt : HasIO io => (dst : CArray8 World n) -> (dstOff : Nat)
           -> (src : CArray8 World m) -> (srcOff : Nat)
           -> (count : Nat) -> io ()
copyBytesAt dst dstOff src srcOff count = go 0
  where
    sp : AnyPtr
    sp = unsafeUnwrap src
    dp : AnyPtr
    dp = unsafeUnwrap dst
    go : Nat -> io ()
    go i =
      if i >= count
        then pure ()
        else do
          let b = rget sp (cast (srcOff + i))
          primIO $ rset dp (cast (dstOff + i)) b
          go (S i)

export covering
zeroBytesAt : HasIO io => (arr : CArray8 World n) -> (off : Nat) -> (count : Nat) -> io ()
zeroBytesAt arr off count = go 0
  where
    ptr : AnyPtr
    ptr = unsafeUnwrap arr
    go : Nat -> io ()
    go i =
      if i >= count
        then pure ()
        else do
          primIO $ rset ptr (cast (off + i)) 0
          go (S i)

export
writeListAt : HasIO io => (arr : CArray8 World n) -> (off : Nat) -> List Bits8 -> io ()
writeListAt arr off [] = pure ()
writeListAt arr off (b :: bs) = do
  primIO $ rset (unsafeUnwrap arr) (cast off) b
  writeListAt arr (S off) bs

------------------------------------------------------------------------
-- Checksum utilities
------------------------------------------------------------------------

export
foldChecksum : Bits32 -> Bits16
foldChecksum acc =
  let folded = (acc `shiftR` 16) + (acc .&. 0xFFFF)
      folded2 = folded + (folded `shiftR` 16)
  in complement (cast folded2)

export covering
onesCompSumAt : HasIO io => (arr : CArray8 World n) -> (off : Nat) -> (len : Nat) -> io Bits32
onesCompSumAt arr off len = go 0 0
  where
    ptr : AnyPtr
    ptr = unsafeUnwrap arr
    go : Nat -> Bits32 -> io Bits32
    go i acc =
      if i >= len
        then pure acc
        else if i + 1 < len
          then do
            let o  = cast {to = Bits64} (off + i)
                hi = rget ptr o
                lo = rget ptr (o + 1)
                w : Bits32 = (cast hi `shiftL` 8) .|. cast lo
            go (i + 2) (acc + w)
          else do
            let o  = cast {to = Bits64} (off + i)
                hi = rget ptr o
            go (i + 1) (acc + (cast hi `shiftL` 8))
