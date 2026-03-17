module Kernel

import Data.C.Array8
import public Control.Monad.Reader
import public Data.Linear.Token

public export
PageTable : Type
PageTable = (n ** CArray8 World n)

public export
Kernel : Type -> Type
Kernel = ReaderT PageTable IO

