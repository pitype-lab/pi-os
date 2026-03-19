module System

import Uart

%foreign "C:exit"
prim_exit : PrimIO ()

export
exit : HasIO io => io ()
exit = primIO prim_exit

export
panic : String -> IO ()
panic msg = do
  println msg
  exit

%foreign "C:enter_supervisor_mode"
prim__enter_smode : Bits64 -> PrimIO ()

export
enterSupervisorMode : HasIO io => Bits64 -> io ()
enterSupervisorMode satp = primIO $ prim__enter_smode satp
