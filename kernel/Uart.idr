module Uart

import MMIO

export
println: String -> IO ()
println xs = println' (unpack xs)
  where 
    println': List Char -> IO ()
    println' [] = write_mmio_bits8 UART $ cast '\n'
    println' (x :: xs) = do
      write_mmio_bits8 UART $ cast x
      println' xs

