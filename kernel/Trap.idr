module Trap

import Data.Bits
import Data.C.Ptr
import Data.Nat
import Lib
import Uart

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


%export "urefc:Trap_m_trap"
export
m_trap : (epc : Nat) -> (tval : Nat) -> (cause : Bits64) -> (hart : Nat) -> (status : Nat) -> IO Nat
m_trap epc tval cause hart status = do
  let cause_num = cast {to=Nat} $ (cast cause) .&. 0xfff
  let in_async = ((shiftR cause 63) .&. 1) == 1
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
    notAsync 2 = do
      panic $ "Illegal instruction CPU#" ++ show hart ++  " -> 0x" ++ b64ToHexString (cast epc) ++ ": 0x" ++ b64ToHexString (cast tval)
      pure epc
    notAsync 7 = do
      println "m_trap"
      let mtimecmp = cast_Bits64AnyPtr 0x02004000 
      let mtime = cast_Bits64AnyPtr 0x0200bff8
      mtime_val <- deref {a=Bits64} mtime
      setPtr mtimecmp (mtime_val + 10000000)
      pure epc
    notAsync 15 = do
      panic $ "Store page fault CPU#" ++ show hart ++ " -> 0x" ++ b64ToHexString (cast epc) ++ ": 0x" ++ b64ToHexString (cast tval) 
      pure epc
    notAsync cause_num = do
      panic $ "Unhandled sys trap CPU#" ++ show hart ++ " -> " ++ show cause_num
      pure epc
