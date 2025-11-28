module Data.C.Ptr.Extra

import Data.C.Ptr

export
Cast Bits8 AnyPtr where
  cast = believe_me

export
Cast Bits16 AnyPtr where
  cast = believe_me

export
Cast Bits32 AnyPtr where
  cast = believe_me

export
Cast Bits64 AnyPtr where
  cast = believe_me

export
Cast (Ptr Bits8) AnyPtr where
  cast = believe_me

export
Cast (Ptr Bits16) AnyPtr where
  cast = believe_me

export
Cast (Ptr Bits32) AnyPtr where
  cast = believe_me

export
Cast (Ptr Bits64) AnyPtr where
  cast = believe_me

export
Cast Bits64 (Ptr Bits8) where
  cast = believe_me

export
Cast Bits64 (Ptr Bits16) where
  cast = believe_me

export
Cast Bits64 (Ptr Bits32) where
  cast = believe_me

export
Cast Bits64 (Ptr Bits64) where
  cast = believe_me

export
Cast AnyPtr (Ptr Bits8) where
  cast = believe_me

export
Cast AnyPtr (Ptr Bits16) where
  cast = believe_me

export
Cast AnyPtr (Ptr Bits32) where
  cast = believe_me

export
Cast AnyPtr (Ptr Bits64) where
  cast = believe_me

export
Cast AnyPtr Bits64 where
  cast = believe_me


export 
incPtr : {a : Type} -> (SizeOf a, Cast (Ptr a) AnyPtr) => Ptr a -> Bits32 -> Ptr a
incPtr {a} ptr inc = believe_me $ prim__inc_ptr (cast {to=AnyPtr} ptr) (sizeof a) inc

export
deref : {a : Type} -> (Deref a, Cast (Ptr a) AnyPtr) => Ptr a -> IO a
deref {a} ptr = Data.C.Deref.deref {a=a} $ cast ptr

export
setPtr : {a : Type} -> (SetPtr a, Cast (Ptr a) AnyPtr) => Ptr a -> a -> IO ()
setPtr {a} ptr val = Data.C.Deref.setPtr {a=a} (cast ptr) val

