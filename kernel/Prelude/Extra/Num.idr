module Prelude.Extra.Num

import public Data.Nat
import public Data.So
import public Data.DPair

export
toDouble : Cast from Double => from -> Double
toDouble = cast

public export
NatPos : Type
NatPos = Subset Nat (\n => GT n 0)

public export
mkNatPos : (n : Nat) -> {auto ok : So (n > 0)} -> NatPos
mkNatPos (S k) = Element (S k) (LTESucc LTEZero)
