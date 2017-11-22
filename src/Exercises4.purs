module Exercise4 where

import Prelude

import Data.Array (null, filter, concatMap, (..))
import Data.Array.Partial (tail, head)
import Partial.Unsafe (unsafePartial)

import Data.Foldable (product)

length :: forall a. Array a -> Int
length arr =
  if null arr
    then 0
    else 1 + length (unsafePartial tail arr)


evenCount :: Array Int -> Int
evenCount arr = evenCount' 0 arr


evenCount' :: Int -> Array Int -> Int
evenCount' n arr =
  if null arr
    then n
    else
      let evenTotal = ((unsafePartial head arr) + 1) `mod` 2
      in evenCount' (n + evenTotal) (unsafePartial tail arr)



infix 0 filter as <$?>

pairs :: Int -> Array (Array Int)
pairs n =
   concatMap (\i ->
     map (\j -> [i, j]) (i..n)
   ) (1..n)


factors :: Int -> Array (Array Int)
factors n = filter (\pair -> product pair == n) (pairs n)
