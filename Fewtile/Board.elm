module Fewtile.Board where

import Fewtile.Tile(Tile)
import Fewtile.Tile as Tile

data Board =
    Stable           [Tile]        -- cur
  | Animating [Tile] [Tile] [Tile] -- src current dest

stabilizeBoard (Stable ts) = Stable ts
stabilizeBoard (Animating _ _ dest) = Stable dest

destabilizeBoard dest (Stable cur) = Animating cur cur dest
destabilizeBoard dest (Animating src cur _) = Animating src cur dest

srcTiles (Stable ts) = ts
srcTiles (Animating ts _ _ ) = ts

curTiles (Stable ts) = ts
curTiles (Animating _ ts _) = ts

destTiles (Stable ts) = ts
destTiles (Animating _ _ ts) = ts
