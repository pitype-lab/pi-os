module Plic

import Data.Bits
import Data.C.Extra
import MMIO

export
set_threshold : HasIO io => Nat -> io ()
set_threshold tsh = do
  let actual_prio = cast {to=Bits32} tsh .&. 7
  write_mmio_bits32 PlicThreshold actual_prio

export
enable : HasIO io => Nat -> io ()
enable id = do
  let actual_id = cast {to=Bits32} $ shiftL 1 id
  enables <- read_mmio_bits32 PlicEnable
  write_mmio_bits32 PlicEnable (enables .|. actual_id)

export
set_priority : HasIO io => Nat -> Nat -> io ()
set_priority id prio = do
  let actual_prio = cast {to=Bits32} prio .&. 7
      addr = mmioAddr PlicPriority + cast id * 4
  primIO $ prim__set_bits32 addr actual_prio
