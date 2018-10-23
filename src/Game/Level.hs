{-# LANGUAGE NamedFieldPuns #-}

module Game.Level (
    Powerup(..),
    Tile(..),
    Table(..),
    Level(..),
    TileMatcher(..),
    Drawable(..),
    Functor(..),
    readLevel,
    (!),
    set
) where

import Prelude hiding (length)
import qualified Data.Vector as Vec
import Engine.Core.Coordinate
import Engine.Graphics.Sprite
import Engine.Graphics.Rendering
import Resources
import Constants
import Debug.Trace


{- Data structures -}
data Powerup = PacDot | PowerPill | Cherry deriving (Show, Eq)

data Tile = TileEmpty | TilePowerup Powerup | TileWall Sprite deriving (Show, Eq)

data IsWallTile = Wall Tile | NotWall Tile deriving (Show, Eq)

data Table a = Table (Vec.Vector (Vec.Vector a)) Int Int deriving (Show, Eq)

data Level = Level (Table Tile) deriving (Show, Eq)

data TileMatcher = TileMatcher {
                      none, up, down, left, right, upLeft, upRight, downLeft, downRight :: IsWallTile
                   } deriving (Show)

{- Classes -}


{- Instances -}
instance Functor Table where
    fmap f (Table vec w h) = Table vecn w h where vecn = Vec.map (Vec.map f) vec

instance Drawable Level where
    draw (Level t@(Table _ w h)) = [drawTile (Pos x y) | x <- [0.. w-1], y <- [0.. h-1]]
                                   where drawTile p = DrawInstruction (newCoord p) (tileToSprite (t ! p))
                                         newCoord (Pos x y) = (Coordinate (fromIntegral x) (fromIntegral y)) * (fromInteger tileSize)

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

createTileMatcher :: Pos -> Level -> TileMatcher
createTileMatcher p (Level t) = TileMatcher{
                                    none      = isWallTile $ t ! (p),
                                    up        = isWallTile $ t ! (p + Pos 0    (-1)),
                                    down      = isWallTile $ t ! (p + Pos 0    1),
                                    left      = isWallTile $ t ! (p + Pos (-1) 0),
                                    right     = isWallTile $ t ! (p + Pos 1    0),
                                    upLeft    = isWallTile $ t ! (p + Pos (-1) (-1)),
                                    upRight   = isWallTile $ t ! (p + Pos 1    (-1)),
                                    downLeft  = isWallTile $ t ! (p + Pos (-1) 1),
                                    downRight = isWallTile $ t ! (p + Pos 1    1)
                                }

isWallTile :: Tile -> IsWallTile
isWallTile t@(TileWall a) = Wall t
isWallTile t = NotWall t

matchWallSprite :: Pos -> TileMatcher -> Tile
matchWallSprite p TileMatcher{none=Wall _, left = Wall _, right = NotWall _, up = NotWall _, down = Wall _, upRight = NotWall _, downLeft = NotWall _} = TileWall spriteTileSCornerLD
matchWallSprite _ TileMatcher{none=Wall _, left = NotWall _, right = Wall _, up = NotWall _, down = Wall _, upLeft = NotWall _, downRight = NotWall _ } = TileWall spriteTileSCornerRD
matchWallSprite _ TileMatcher{none=Wall r} = r
matchWallSprite _ TileMatcher{none=NotWall r} = r

-- Split this into level loading
-- | Reads raw level into a character map
readRawLevel :: String -> IO [[Char]]
readRawLevel file = do contents <- readFile file
                       return (lines contents)

-- | Parses a tile from a char
parseTile :: Char -> Tile
parseTile char = case char of '#' -> TileWall createEmptySprite
                              '.' -> TilePowerup PacDot
                              '@' -> TilePowerup PowerPill
                              _   -> TileEmpty

-- | Creates table from given list of lists of a type
createTable :: [[a]] -> Table a
createTable input = Table tableData width height
                    where tableData = Vec.fromList (map Vec.fromList input)
                          height = Vec.length tableData
                          width | height > 0 = Vec.length (tableData Vec.! 1)
                                | otherwise  = 0

-- | Parses level from character table
parseLevel :: [[Char]] -> Level
parseLevel rawLevel = Level (createTable tiles)
                      where tiles = map (map parseTile) rawLevel

updateLevelWalls :: Level -> Level
updateLevelWalls l@(Level (Table vec w h)) = Level (Table nvec w h)
                                             where nvec = Vec.fromList [nrow y | y <- [0.. h-1]]
                                                   nrow y = Vec.fromList [ntile (Pos x y) | x <- [0.. w-1]]
                                                   ntile p = matchWallSprite p (createTileMatcher p l)

-- | Reads level from a file. Warning IO
readLevel :: String -> IO Level
readLevel file = do rawLevel <- readRawLevel file
                    return (updateLevelWalls (parseLevel rawLevel))
