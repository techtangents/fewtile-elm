module Fewtile.Tile where

data Tile = Tile
  String     -- id
  String     -- text
  (Int, Int) -- origin i.e. top-left. (x, y)
  (Int, Int) -- size (width, height)

tileId     (Tile q _ _ _) = q
tileText   (Tile _ q _ _) = q
tileOrigin (Tile _ _ q _) = q
tileSize   (Tile _ _ _ q) = q

tileX      (Tile _ _ (q, _) _) = q
tileY      (Tile _ _ (_, q) _) = q
tileWidth  (Tile _ _ _ (q, _)) = q
tileHeight (Tile _ _ _ (_, q)) = q

