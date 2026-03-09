module System

import Uart
import Control.App

%foreign "C:exit"
prim_exit : PrimIO ()

export
exit : IO ()
exit = primIO prim_exit

export
panic : Has [HasUart,PrimIO] e => String -> App e ()
panic msg = do
  putStrLn msg
  primIO exit
