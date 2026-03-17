module Prelude.Extra.Num

import public Data.Nat
import public Data.So

export
toDouble : Cast from Double => from -> Double
toDouble = cast

public export
NatPos : Type
NatPos = (n : Nat ** GT n 0)

public export
mkNatPos : (n : Nat) -> {auto ok : So (n > 0)} -> NatPos
mkNatPos (S k) = (S k ** LTESucc LTEZero)
