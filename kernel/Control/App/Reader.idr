module Control.App.Reader

import public Control.App

public export
data Reader : (tag : a) -> Type -> List Error -> Type where
     [search tag]
     MkReader : t -> Reader tag t e

%hint export
mapReader : Reader tag t e -> Reader tag t (eff :: e)
mapReader (MkReader val) = MkReader val

export
ask : (0 tag : _) -> Reader tag t e => App {l} e t
ask tag @{MkReader val} = pure val

export
env : t -> (Reader tag t e => App {l} e a) -> App {l} e a
env val prog = prog @{MkReader val}
