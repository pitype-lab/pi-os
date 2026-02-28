module Prelude.Extra.Num

export
Fractional Bits64 where
  (/) a b = cast {to=Bits64} $ (cast {to=Double} a) / (cast {to=Double} b)

export
Fractional Nat where
  (/) a b = cast {to=Nat} $ (cast {to=Double} a) / (cast {to=Double} b)

