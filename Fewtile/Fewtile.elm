module Fewtile.Fewtile where

import List (fromList)
import HTTP
import Dict (diff)
import Time (every)
import Window as Window

-- (Ord k, Eq v) =>
data Op k v = Add k v | Remove k | Change k v v

-- opKey :: Op k v -> k
opKey (Add    k _   ) = k
opKey (Remove k     ) = k
opKey (Change k _ _ ) = k

-- opValue :: Op k v -> v
opValue (Add    _ v   ) = Just v
opValue (Remove _     ) = Nothing
opValue (Change _ _ v ) = Just v

-- index :: (v -> k) -> [v] -> Dict k v
index f xs = fromList $ zip (map f xs) xs

-- diffo :: Dict k v -> [v] -> (Dict k v, [Op k v])
diffo ixer olds =
  let (addChanges, nus) = foldl
    (\(m, ops) nu ->
      let k = ixer nu
      in let old = lookup k olds
      in
        maybe
         (insert m k nu, Add k nu)
         (if (old == nu)
             then (m, ops)
             else (insert m k nu, (Change k nu) : ops)
        )
    )
    (empty, [])
  in let removes = map (\(k, v) -> Remove k) (toList (olds, nus))
  in (removes ++ addChanges, nus)

data BoardState = Stable | Animating

-- a -> b -> a
const x _ = x

-- a -> b -> b
tsonc _ x = x

-- b -> Signal a -> Signal b
lc = lift . const

-- a -> Signal a -> Signal b
lt = lift . tsonc

mapPair f (a, b) = (f a, f b)

showPair p =
  let (a, b) = mapPair show p
  in "(" ++ a ++ "," ++ b ++ ")"

getty2 = sendGet $ lc "/api/json?tree=jobs[name,color]" $ every 3

printCode a =
  case a of
    { Waiting -> "..."
    ; (Success s) -> s
    ; (Failure i s) -> show i ++ s
    }

-- main = lift (plainText . printCode) $ getty2

-- main = lift (plainText . showPair) Window.dimensions

fst (a, _) = a
snd (_, a) = a

main =
  lift3 collage Window.width Window.height $ constant [
    filled (rgb 0 255 0) (rect 100 100 (150, 150))
  ]




