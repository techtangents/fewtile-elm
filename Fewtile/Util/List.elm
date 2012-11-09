module Fewtile.Util.List where

import List (concatMap)

-- liftList2 :: (a -> b -> c) -> [a] -> [b] -> [c]
liftList2 f list1 list2 =
  concatMap (\x1 -> concatMap (\x2 -> [f x1 x2]) list2) list1
