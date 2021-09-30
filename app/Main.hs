module Main where

import Toysim

main :: IO ()
main = interact (wrap (toy sampleCode))

wrap :: ([String] -> [String]) -> (String -> String)
wrap f = unlines . f . lines

