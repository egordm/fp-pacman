module Game.Level.Level (
    Powerup(..),
    Tile(..),
    Table(..),
    Level(..),
    (!),
    set,
    tileToCoordinate,
    coordinateToTile
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
    draw l@(Level t@(Table _ w h)) = [drawTile (Pos x y) | x <- [0.. w-1], y <- [0.. h-1]]
                                     where drawTile p = DrawInstruction (tileToCoordinate l p) (tileToSprite (t ! p))

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

tileToCoordinate :: Level -> Pos -> Coordinate
tileToCoordinate Level{tiles=Table _ w h} (Pos x y) = (coordinate x y - coordinate w h / 2) * fromInteger tileSize

coordinateToTile :: Level -> Coordinate -> Pos
coordinateToTile Level{tiles=Table _ w h} c = coordinateToPos (c / fromInteger tileSize + coordinate w h / 2)
