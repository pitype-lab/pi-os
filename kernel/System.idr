module System

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
