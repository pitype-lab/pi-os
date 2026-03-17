module Uart

import MMIO

export
println : HasIO io =>  String -> io ()
println xs = println' (unpack xs)
  where
    println' : List Char -> io ()
    println' [] = write_mmio_bits8 UART $ cast '\n'
    println' (x :: xs) = do
      write_mmio_bits8 UART $ cast x
      println' xs

