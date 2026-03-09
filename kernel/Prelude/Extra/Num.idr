module Prelude.Extra.Num

import public Data.So

export
toDouble : Cast from Double => from -> Double
toDouble = cast

public export
NatPos : Type
NatPos = (n : Nat ** So (n > 0))

export
mkNatPos : (n : Nat) -> {auto prf : So (n > 0)} -> NatPos
mkNatPos n = (n ** prf)
