module Prelude.Extra.Num

import Data.So

export
toDouble : Bits64 -> Double
toDouble x = cast {to=Double} x

public export
NatPos : Type
NatPos = (n : Nat ** So (n > 0))

export
mkNatPos : (n : Nat) -> {auto prf : So (n > 0)} -> NatPos
mkNatPos n = (n ** prf)
