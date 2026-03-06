module Main

import Data.List
import Pages
import System
import Uart

%export "urefc:Main_kinit"
kinit : IO ()
kinit = println "Init PI OS memory"

main : IO ()
main = do
  println "Welcome to PI-OS!"
  (Just pagesWithLenghtPrf) <- pure kpages
      | Nothing => panic "Failed to initialize pages"
  let pages = getPages pagesWithLenghtPrf
  println $ "Number of pages: " ++ show numPages
  println $ "Pages: " ++ (show $ take 10 pages)
  exit





