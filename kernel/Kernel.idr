module Kernel

import Data.C.Array8
import public Control.Monad.Reader
import public Data.Linear.Token
import public Heap

-- Phase 1: init-only monad, holds just the page table (no root yet)
public export
InitKernel : Nat -> Type -> Type
InitKernel n = ReaderT (CArray8 World n) IO

export
runInitKernel : CArray8 World n -> InitKernel n a -> IO a
runInitKernel pt app = runReaderT pt app

-- Phase 2: full kernel monad, holds page table + MMU root
public export
record KernelEnv (n : Nat) where
  constructor MkKernelEnv
  pageTable : CArray8 World n
  root : HeapAddr

public export
Kernel : Nat -> Type -> Type
Kernel n = ReaderT (KernelEnv n) IO

export
getPageTable : Kernel n (CArray8 World n)
getPageTable = asks pageTable

export
getRoot : Kernel n HeapAddr
getRoot = asks root

export
runKernel : KernelEnv n -> Kernel n () -> IO ()
runKernel env app = runReaderT env app

-- Lift an InitKernel action into Kernel by extracting the page table
export
liftInit : InitKernel n a -> Kernel n a
liftInit action = do
  pt <- asks pageTable
  liftIO $ runReaderT pt action
