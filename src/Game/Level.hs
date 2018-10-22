module Game.Level () where

import Prelude hiding (length)
import qualified Data.Vector as Vec
import Engine.Core.Coordinate
import Engine.Graphics.Sprite


{- Data structures -}
data Powerup = PacDot | PowerPill | Cherry deriving (Show)

data Tile = TileEmpty | TilePowerup Powerup | TileWall Sprite deriving (Show)

data Table a = Table (Vec.Vector (Vec.Vector a)) Int Int deriving (Show)

data Level = Level (Table Tile) deriving (Show)

data Pos = Pos Int Int

{- Classes -}


{- Instances -}


{- Functions -}
(!) :: Table Tile -> Pos -> Tile
(!) (Table vec w h) (Pos x y) | x < 0 || x >= w || y < 0 || y >= h = TileEmpty
                              | otherwise = vec Vec.! y Vec.! x

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

-- | Reads level from a file. Warning IO
readLevel :: String -> IO Level
readLevel file = do rawLevel <- readRawLevel file
                    return (parseLevel rawLevel)
