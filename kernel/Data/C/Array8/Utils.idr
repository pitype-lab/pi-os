module Data.C.Array8.Utils

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
||| applicative action, discarding results. Mirrors traverseWithIndex from idris2-array.
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
