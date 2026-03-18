module Data.C.Array8.Utils

import Data.C.Array8
import public Data.Linear.Token
import Data.Nat
import Data.Array.Index

%default total

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
