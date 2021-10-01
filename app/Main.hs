module Main where

import System.Environment
import Toysim ( sampleCode, toysim )


-- main :: IO ()
-- main = toysim sampleCode

main :: IO ()
main = toysim =<< readFile . head =<< getArgs

