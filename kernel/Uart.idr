module Uart

import MMIO
import Control.App

uartPutStrLn : String -> IO ()
uartPutStrLn xs = println' (unpack xs)
  where
    println' : List Char -> IO ()
    println' [] = write_mmio_bits8 UART $ cast '\n'
    println' (x :: xs) = do
      write_mmio_bits8 UART $ cast x
      println' xs

public export
interface HasUart e where
  putStrLn : String -> App {l} e ()

export
PrimIO e => HasUart e where
  putStrLn s = primIO $ uartPutStrLn s

