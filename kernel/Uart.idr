module Uart

import MMIO
import Prelude.Extra

export
println: String -> IO ()
println xs = println' (unpack xs)
  where 
    println': List Char -> IO ()
    println' [] = write_mmio_bits8 UART '\n'
    println' (x :: xs) = do
      write_mmio_bits8 UART $ fromChar x
      println' xs

