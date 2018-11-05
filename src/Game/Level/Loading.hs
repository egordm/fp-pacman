module Game.Level.Loading (
    readLevel
) where

import qualified Data.Vector as Vec
import Engine.Core.Base
import Engine.Graphics.Base
import Game.Level.Level
import Resources
import Constants
import Data.List

{- Data structures -}
data IsWallTile = Wall Tile | NotWall Tile deriving (Show, Eq)


data TileMatcher = TileMatcher {
                      none, up, down, left, right, upLeft, upRight, downLeft, downRight :: IsWallTile
                   } deriving (Show)


{- Classes -}

{- Instances -}

{- Functions -}
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
                              '_' -> TileMarker (Marker '_')
                              'u' -> TileWall spriteTileSStraightU
                              'd' -> TileWall spriteTileSStraightD
                              'l' -> TileWall spriteTileSStraightL
                              'r' -> TileWall spriteTileSStraightR
                              _   -> otherTile char
                              where otherTile c | c `elem` markerChars = TileMarker (Marker c)
                                                | otherwise            = TileEmpty

markersSingular = '_' : ['A'.. 'Z']
markersMultiple = ['0'.. '9']
markerChars = markersSingular ++ markersMultiple

-- | Creates table from given list of lists of a type
createTable :: [[a]] -> Table a
createTable input = Table tableData width height
                    where tableData = Vec.fromList (map Vec.fromList input)
                          height = Vec.length tableData
                          width | height > 0 = Vec.length (tableData Vec.! 1)
                                | otherwise  = 0

-- | Parses level from character table
parseLevel :: [[Char]] -> Level
parseLevel rawLevel = Level (createTable tiles) []
                      where tiles = map (map parseTile) rawLevel

-- | Updates walls by matching a sprite to their surroundings
updateWalls :: Table Tile -> Table Tile
updateWalls t@(Table vec w h) = Table nvec w h
                                     where nvec = Vec.fromList [nrow y | y <- [0.. h-1]]
                                           nrow y = Vec.fromList [ntile (Pos x y) | x <- [0.. w-1]]
                                           ntile p = matchWallSprite (createTileMatcher p t)

-- | Processes level by initializing all the tiles and extracting markers
processLevel :: Level -> Level
processLevel level = level{
                        tiles = updateWalls (updateWalls (tiles level)),
                        markers = extractMarkers (tiles level)
                     }

-- | Reads level from a file. Warning IO
readLevel :: String -> IO Level
readLevel file = do rawLevel <- readRawLevel file
                    return (processLevel (parseLevel rawLevel))

-- | Reads markers from the level and averages their position of more of the same is placed.
extractMarkers :: Table Tile -> [(Marker, Coordinate)]
extractMarkers t@(Table vec w h) = foldr (++) [] (map mergeMarkers groupedMarkerTiles)
                                   where markerTiles  = [(marker x y, tileToCoordinate t (Pos x y)) | x <- [0.. w-1], y <- [0.. h-1], isMarker x y]
                                         isMarker x y = case (t!(Pos x y)) of TileMarker _ -> True; _ -> False
                                         marker x y = case (t!(Pos x y)) of TileMarker m -> m; _ -> Marker ' '
                                         sortedMarkerTiles = sortBy (\a b -> compare (fst a) (fst b)) markerTiles
                                         groupedMarkerTiles = groupBy (\a b -> fst a == fst b) sortedMarkerTiles

-- | Averages given markers by position
averageMarkers :: [(Marker, Coordinate)] -> (Marker, Coordinate)
averageMarkers ms = let (m, c) = foldr (\(fa, sa) (fb, sb) -> (fa, sa + sb)) (Marker ' ', coordZ) ms
                    in (m, c / fromIntegral (length ms))

-- | Merges given markers if they are singular. Otherwise nothing is done
mergeMarkers :: [(Marker, Coordinate)] -> [(Marker, Coordinate)]
mergeMarkers [] = []
mergeMarkers ms@((Marker m, _):_) | m `elem` markersSingular = [averageMarkers ms]
                                  | otherwise = ms

-- LEVEL DECORATION =====================================

-- | Creates a table matcher to match the tile
createTileMatcher :: Pos -> Table Tile -> TileMatcher
createTileMatcher p t = TileMatcher{
                            none      = isWallTile $ t ! p,
                            up        = isWallTile $ t ! (p + Pos 0    (-1)),
                            down      = isWallTile $ t ! (p + Pos 0    1),
                            left      = isWallTile $ t ! (p + Pos (-1) 0),
                            right     = isWallTile $ t ! (p + Pos 1    0),
                            upLeft    = isWallTile $ t ! (p + Pos (-1) (-1)),
                            upRight   = isWallTile $ t ! (p + Pos 1    (-1)),
                            downLeft  = isWallTile $ t ! (p + Pos (-1) 1),
                            downRight = isWallTile $ t ! (p + Pos 1    1)
                        }
-- | Check if tile is a wall
isWallTile :: Tile -> IsWallTile
isWallTile t@(TileWall a) = Wall t
isWallTile t = NotWall t


-- This is an abomination. Do I need to say more? Still though spriting while making level is for losers
matchWallSprite :: TileMatcher -> Tile
matchWallSprite TileMatcher{none=Wall _, left = Wall lw, right = NotWall _, up = Wall uw, down = NotWall _}
    | lw == TileWall spriteTileSStraightD && uw == TileWall spriteTileSStraightR = TileWall spriteTileSCornerLU
    | otherwise = TileWall spriteTileFCornerLUUL
matchWallSprite TileMatcher{none=Wall _, left = NotWall _, right = Wall rw, up = Wall uw, down = NotWall _}
    | rw == TileWall spriteTileSStraightD && uw == TileWall spriteTileSStraightL = TileWall spriteTileSCornerRU
    | otherwise = TileWall spriteTileFCornerRUUR
matchWallSprite TileMatcher{none=Wall _, left = Wall lw, right = NotWall _, up = NotWall _, down = Wall dw}
    | lw == TileWall spriteTileSStraightU && dw == TileWall spriteTileSStraightR = TileWall spriteTileSCornerLD
    | otherwise = TileWall spriteTileFCornerLDDL
matchWallSprite TileMatcher{none=Wall _, left = NotWall _, right = Wall rw, up = NotWall _, down = Wall dw}
    | rw == TileWall spriteTileSStraightU && dw == TileWall spriteTileSStraightL = TileWall spriteTileSCornerRD
    | otherwise = TileWall spriteTileFCornerRDDR

matchWallSprite TileMatcher{none=Wall _, left = Wall _, right = NotWall _, up = Wall _, down = Wall _, upRight = NotWall _, upLeft = NotWall _} = TileWall spriteTileSSplitUDL
matchWallSprite TileMatcher{none=Wall _, left = NotWall _, right = Wall _, up = Wall _, down = Wall _, upRight = NotWall _, upLeft = NotWall _} = TileWall spriteTileSSplitUDR
matchWallSprite TileMatcher{none=Wall _, left = Wall _, right = NotWall _, up = Wall _, down = Wall _, downRight = NotWall _, downLeft = NotWall _} = TileWall spriteTileSSplitDUL
matchWallSprite TileMatcher{none=Wall _, left = NotWall _, right = Wall _, up = Wall _, down = Wall _, downRight = NotWall _, downLeft = NotWall _} = TileWall spriteTileSSplitDUR
matchWallSprite TileMatcher{none=Wall _, left = Wall _, right = Wall _, up = NotWall _, down = Wall _, downRight = Wall _, downLeft = NotWall _} = TileWall spriteTileSSplitLRD
matchWallSprite TileMatcher{none=Wall _, left = Wall _, right = Wall _, up = NotWall _, down = Wall _, downRight = NotWall _, downLeft = Wall _} = TileWall spriteTileSSplitRLD

matchWallSprite TileMatcher{none=Wall _, left = Wall _, upLeft = NotWall _, up = Wall _} = TileWall spriteTileFCornerLUUL
matchWallSprite TileMatcher{none=Wall _, right = Wall _, upRight = NotWall _, up = Wall _} = TileWall spriteTileFCornerRUUR
matchWallSprite TileMatcher{none=Wall _, left = Wall _, downLeft = NotWall _, down = Wall _} = TileWall spriteTileFCornerLDDL
matchWallSprite TileMatcher{none=Wall _, right = Wall _, downRight = NotWall _, down = Wall _} = TileWall spriteTileFCornerRDDR

matchWallSprite TileMatcher{none=Wall _, left = Wall _, right = Wall _, up = NotWall _, down = Wall _} = TileWall spriteTileFStraightD
matchWallSprite TileMatcher{none=Wall _, left = Wall _, right = Wall _, up = Wall _, down = NotWall _} = TileWall spriteTileFStraightU
matchWallSprite TileMatcher{none=Wall _, left = Wall _, right = NotWall _, up = Wall _, down = Wall _} = TileWall spriteTileFStraightL
matchWallSprite TileMatcher{none=Wall _, left = NotWall _, right = Wall _, up = Wall _, down = Wall _} = TileWall spriteTileFStraightR

matchWallSprite TileMatcher{none=Wall _, left = Wall lw, right = NotWall TileEmpty, up = NotWall _, down = NotWall _, upLeft = NotWall _, downLeft = NotWall _} = lw
matchWallSprite TileMatcher{none=Wall _, left = NotWall TileEmpty, right = Wall rw, up = NotWall _, down = NotWall _, upRight = NotWall _, downRight = NotWall _} = rw

matchWallSprite TileMatcher{none=Wall _, left = Wall lw, right = NotWall (TileMarker (Marker '_')), up = NotWall _, down = NotWall _, upLeft = NotWall _, downLeft = NotWall _} = TileWall spriteTileCEndR
matchWallSprite TileMatcher{none=Wall _, left = NotWall (TileMarker (Marker '_')), right = Wall rw, up = NotWall _, down = NotWall _, upRight = NotWall _, downRight = NotWall _} = TileWall spriteTileCEndL

matchWallSprite TileMatcher{none=Wall r} = r
matchWallSprite TileMatcher{none=NotWall r} = r