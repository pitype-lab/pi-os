module Data.String.Extra

import Data.Bits

export
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


