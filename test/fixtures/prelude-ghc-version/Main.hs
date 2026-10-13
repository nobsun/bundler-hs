module Main (main) where

import Fold (total)

main :: IO ()
main = print (total [1, 2, 3])
