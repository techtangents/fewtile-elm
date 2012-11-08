module Fewtile.Easing where

-- Easing function for an animation.
-- An easing is a function from % to %.

data Easing = Easing (Float -> Float)

-- Calculate the percentage of the delta we should traverse at this point in time
easingPercent (Easing f) total elapsed = f (elapsed / total)

-- Calculate the value for this point in time
easingDelta e total elapsed src dest = src + ((easingPercent e total elapsed) * (dest - src))

-- utils
bound = clamp 0 1
beas f = Easing (bound . f)
floatCeil = toFloat . ceiling

-- instances

linear = Easing id
exponential = Easing (\x -> x * x)
inverse = Easing (\x -> floatCeil (1 / x))
