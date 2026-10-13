-- Prelude exports foldl' only since base 4.20 (GHC 9.10). A bundle built
-- for GHC 9.8 has nothing to move this name out of the way of.
module Fold (foldl', total) where

import Data.List qualified as L

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' = L.foldl'

total :: [Int] -> Int
total = L.foldl' (+) 0
