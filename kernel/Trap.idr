module Trap

import Data.Bits
import Data.C.Ptr
import Data.Nat

import Uart

cast_Bits64AnyPtr: Bits64 -> AnyPtr
cast_Bits64AnyPtr = believe_me

%foreign "C:exit"
prim_exit : PrimIO ()

export
exit : IO ()
exit = primIO prim_exit

export
panic : String -> IO ()
panic msg = do
  println msg
  exit

private
b64ToHexString : Bits64 -> String
b64ToHexString n =
  case n of
    0 => "0"
    1 => "1"
    2 => "2"
    3 => "3"
    4 => "4"
    5 => "5"
    6 => "6"
    7 => "7"
    8 => "8"
    9 => "9"
    10 => "A"
    11 => "B"
    12 => "C"
    13 => "D"
    14 => "E"
    15 => "F"
    other => assert_total $
               b64ToHexString (n `shiftR` 4) ++
               b64ToHexString (n .&. 15)


%export "urefc:Trap_m_trap"
export
m_trap : (epc : Nat) -> (tval : Nat) -> (cause : Nat) -> (hart : Nat) -> (status : Nat) -> IO Nat
m_trap epc tval cause hart status = do
  let cause_num = cast {to=Nat} $ (cast cause) .&. 0xfff
  let in_async = ((shiftR (cast cause) 63) .&. 1) == 1
  if in_async 
     then inAsync $ cast cause_num
     else notAsync $ cast cause_num
  
  where
    inAsync : Nat -> IO Nat
    inAsync 7 = do
      let mtimecmp = cast_Bits64AnyPtr 0x02004000 
      let mtime = cast_Bits64AnyPtr 0x0200bff8
      mtime_val <- deref {a=Bits64} mtime
      setPtr mtimecmp (mtime_val + 10000000)
      pure epc
    inAsync cause_num = do
      panic $ "Unhandled async trap CPU#" ++ show hart ++ " -> " ++ show cause_num
      pure epc

    notAsync : Nat -> IO Nat 
    notAsync 15 = do
      panic $ "Store page fault CPU#" ++ show hart ++ " -> 0x" ++ b64ToHexString (cast epc) ++ ": 0x" ++ b64ToHexString (cast tval) 
      pure epc
    notAsync cause_num = do
      panic $ "Unhandled sys trap CPU#" ++ show hart ++ " -> " ++ show cause_num
      pure epc
