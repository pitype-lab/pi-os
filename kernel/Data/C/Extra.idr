module Data.C.Extra

export
%foreign "C:cptr_deref_bits8, cptr-idris"
prim__deref_bits8 : Bits64 -> PrimIO Bits8

export
%foreign "C:cptr_deref_bits16, cptr-idris"
prim__deref_bits16 : Bits64 -> PrimIO Bits16

export
%foreign "C:cptr_deref_bits32, cptr-idris"
prim__deref_bits32 : Bits64 -> PrimIO Bits32

export
%foreign "C:cptr_deref_bits64, cptr-idris"
prim__deref_bits64 : Bits64 -> PrimIO Bits64

export
%foreign "C:cptr_set_bits8, cptr-idris"
prim__set_bits8 : Bits64 -> Bits8 -> PrimIO ()

export
%foreign "C:cptr_set_bits16, cptr-idris"
prim__set_bits16 : Bits64 -> Bits16 -> PrimIO ()

export
%foreign "C:cptr_set_bits32, cptr-idris"
prim__set_bits32 : Bits64 -> Bits32 -> PrimIO ()

export
%foreign "C:cptr_set_bits64, cptr-idris"
prim__set_bits64 : Bits64 -> Bits64 -> PrimIO ()
