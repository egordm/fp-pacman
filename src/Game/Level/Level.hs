module Game.Level.Level (
    Powerup(..),
    Tile(..),
    Table(..),
    Level(..),
    (!),
    set
) where
    
import Prelude hiding (length)
import qualified Data.Vector as Vec
import Engine.Graphics.Sprite
import Engine.Graphics.Rendering
import Engine.Core.Coordinate
import Resources
import Constants
    
{- Data structures -}
data Table a = Table (Vec.Vector (Vec.Vector a)) Int Int deriving (Show, Eq)

data Powerup = PacDot | PowerPill | Cherry deriving (Show, Eq)

data Tile = TileEmpty | TilePowerup Powerup | TileWall Sprite | TileDoor deriving (Show, Eq)

data Level = Level {tiles :: Table Tile} deriving (Show, Eq)

{- Classes -}


{- Instances -}
instance Functor Table where
    fmap f (Table vec w h) = Table vecn w h where vecn = Vec.map (Vec.map f) vec

instance Drawable Level where
    draw (Level t@(Table _ w h)) = [drawTile (Pos x y) | x <- [0.. w-1], y <- [0.. h-1]]
                                   where drawTile p = DrawInstruction (newCoord p) (tileToSprite (t ! p))
                                         newCoord (Pos x y) = (Coordinate (cx x) (cy y)) * fromInteger tileSize
                                         cx x = realToFrac x - realToFrac w/2
                                         cy y = realToFrac y - realToFrac h/2

{- Functions -}
(!) :: Table Tile -> Pos -> Tile
(!) (Table vec w h) (Pos x y) | x < 0 || x >= w || y < 0 || y >= h = TileEmpty
                              | otherwise = vec Vec.! y Vec.! x

set :: Table a -> Pos -> a -> Table a
set t@(Table vec w h) (Pos x y) v | x < 0 || x >= w || y < 0 || y >= h = t
                                  | otherwise = Table vec x y
                                    where nvec = Vec.update vec (Vec.singleton (y, nrow))
                                          nrow = Vec.update (vec Vec.! y) (Vec.singleton (x, v))

tileToSprite :: Tile -> Sprite
tileToSprite tile = case tile of TileEmpty               -> createEmptySprite
                                 (TilePowerup PacDot)    -> spritePowerupPacDot
                                 (TilePowerup PowerPill) -> spritePowerupPowerPellet
                                 (TilePowerup Cherry)    -> spritePowerupCherry
                                 (TileWall sprite)       -> sprite
                                 TileDoor              -> createEmptySprite
