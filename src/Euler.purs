module Euler (answer) where

import Data.Foldable (sum)
import Data.List (range, filter, List)
import Prelude (mod, (==), (||))

ns :: List Int
ns = range 0 999


multiples :: List Int
multiples = filter (\n -> mod n 3 == 0 || mod n 5 == 0) ns

answer :: Int
answer = sum multiples
