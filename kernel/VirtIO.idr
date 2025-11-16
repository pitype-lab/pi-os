module VirtIO

import Data.C.Ptr
import Uart
import Data.Bits

MMIO_VIRTIO_START : Bits32
MMIO_VIRTIO_START = 0x10001000

MMIO_VIRTIO_END : Bits32
MMIO_VIRTIO_END = 0x10008000

MMIO_VIRTIO_STATUS : Bits32
MMIO_VIRTIO_STATUS =  0x70

MMIO_VIRTIO_STRIDE : Bits32
MMIO_VIRTIO_STRIDE =  0x1000

MMIO_VIRTIO_MAGIC : Bits32
MMIO_VIRTIO_MAGIC = 0x74726976

cast_Bits64AnyPtr: Bits64 -> AnyPtr
cast_Bits64AnyPtr = believe_me

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

setupNetwork : IO ()
setupNetwork = pure ()

setup : Bits32 -> IO ()
setup 1 = do
  println "Setup network"
  setupNetwork

setup n = println $ "Virtio " ++ show n ++ "not implemented yet" 

export
probe : IO ()
probe = do
  for_ [MMIO_VIRTIO_START, MMIO_VIRTIO_START+MMIO_VIRTIO_STRIDE..MMIO_VIRTIO_END] $ \addr => do
    magicvalue <- deref {a=Bits32} (cast_Bits64AnyPtr $ cast addr)
    deviceid <- deref {a=Bits32} (cast_Bits64AnyPtr $ cast addr+8)

    when (magicvalue /= MMIO_VIRTIO_MAGIC) $ do
      println "Not virtio"

    if deviceid == 0
      then println "Not connected"
      else setup deviceid
    
    pure ()

