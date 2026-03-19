module Kernel

import Data.C.Array8
import public Control.Monad.Reader
import public Data.Linear.Token

public export
Kernel : Nat -> Type -> Type
Kernel n = ReaderT (CArray8 World n) IO

export
runKernel : CArray8 World n -> Kernel n () -> IO ()
runKernel pageTable app = runReaderT pageTable app
