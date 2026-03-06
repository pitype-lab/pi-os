module Main

import Data.List
import Data.String.Extra
import Memory
import Pages
import System
import Uart

%export "urefc:Main_kinit"
kinit : IO ()
kinit = println "Init PI OS memory"

main : IO ()
main = do
  println "Welcome to PI-OS!"
  (Just pagesWithLengthPrf) <- pure kpages
      | Nothing => panic "Failed to initialize pages"
  let pages = getPages pagesWithLengthPrf
  println $ "Number of pages: " ++ show numPages
  println $ "Pages: " ++ (show $ take 10 pages)
  println $ "Heap start: " ++ b64ToHexString heapStart
  
  exit





