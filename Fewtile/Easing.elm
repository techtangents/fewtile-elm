module Fewtile.Easing where

import Fewtile.Util.List

-- Easing function for an animation.
-- An easing is a function from % to %.

data Easing = Easing (Float -> Float)

-- utils
bound = clamp 0 1
beas f = Easing (bound . f)
floatCeil = toFloat . ceiling

-- instances

linear = Easing id
exponential = Easing (\x -> x * x)
inverse = Easing (\x -> floatCeil (1 / x))

-- resolution

easingPercent (Easing f) total elapsed =
  f (elapsed / total)

easingNum curPc src dest =
  src + (curPc * (dest - src))

easingPair curPc (x, y) (x', y') =
  (easingNum curPc x x', easingNum curPc y y')

easingColor curPc (Color r g b a) (Color r' g' b' a') =
  let en = easingNum curPc
  in Color (en r r') (en g g') (en b b') (en a a')

easingTile curPc (Tile id text origin size color) (Tile _ _ origin' size' color') =
  let ep = easingPair curPc
  in Tile id text (ep origin origin') (ep size size') (easingColor color color')

easingTile' easing total elapsed tile tile' =
  easingTile (easingPercent easing total elapsed) tile tile'

easingTiles' easing total elapsed =
  liftList2 (easingTile' easing total elapsed)


-- easingBoard :: Easing -> Number -> Number -> Board -> Board
easingBoard easing total elapsed board =
  if elapsed < total
    then let { src  = srcTiles board
             ; dest = destTiles board
             ; cur = easingTiles' easing total elapsed src dest
             }
          in Animating src cur dest
    else board
