module Main

import Uart
import Pages 

%export "urefc:Main_kinit"
kinit : IO ()
kinit = println "Init PI OS memory"

main : IO ()
main = println "Welcome to PI-OS!"





