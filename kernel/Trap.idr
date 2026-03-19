module Trap

import Data.Bits
import Data.String.Extra
import MMIO
import Plic
import System
import Uart

%export "urefc:Trap_m_trap"
export
m_trap : (epc : Bits64) -> (tval : Bits64) -> (cause : Bits64) -> (hart : Bits64) -> (status : Bits64) -> IO Bits64
m_trap epc tval cause hart status = do
  let causeNum = cause .&. 0xfff
  let isAsync = (shiftR cause 63 .&. 1) == 1
  if isAsync
     then handleAsync causeNum
     else handleSync causeNum

  where
    handleAsync : Bits64 -> IO Bits64
    handleAsync 7 = do
      mtimeVal <- read_mmio_bits64 ClintMtime
      write_mmio_bits64 ClintMtimecmp (mtimeVal + 10000000)
      pure epc
    handleAsync 11 = do
      irq <- claim
      println "Welcome to the trap function"
      complete irq
      pure epc
    handleAsync n = do
      panic $ "Unhandled async trap CPU#" ++ show hart ++ " -> " ++ show n
      pure epc

    handleSync : Bits64 -> IO Bits64
    handleSync 2 = do
      panic $ "Illegal instruction CPU#" ++ show hart
           ++ " -> 0x" ++ b64ToHexString epc
           ++ ": 0x" ++ b64ToHexString tval
      pure epc
    handleSync 5 = do
      panic $ "Load access fault CPU#" ++ show hart
           ++ " -> 0x" ++ b64ToHexString epc
           ++ ": 0x" ++ b64ToHexString tval
      pure epc
    handleSync 7 = do
      mtimeVal <- read_mmio_bits64 ClintMtime
      write_mmio_bits64 ClintMtimecmp (mtimeVal + 10000000)
      pure epc
    handleSync 12 = do
      panic $ "Instruction page fault CPU#" ++ show hart
           ++ " -> 0x" ++ b64ToHexString epc
           ++ ": 0x" ++ b64ToHexString tval
      pure epc
    handleSync 13 = do
      panic $ "Load page fault CPU#" ++ show hart
           ++ " -> 0x" ++ b64ToHexString epc
           ++ ": 0x" ++ b64ToHexString tval
      pure epc
    handleSync 15 = do
      panic $ "Store page fault CPU#" ++ show hart
           ++ " -> 0x" ++ b64ToHexString epc
           ++ ": 0x" ++ b64ToHexString tval
      pure epc
    handleSync n = do
      panic $ "Unhandled sync trap CPU#" ++ show hart ++ " -> " ++ show n
      pure epc
