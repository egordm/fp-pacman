module Engine.Core.Coordinate (
    Coordinate(..),
    Direction(..),
    fromFloat,
    screenSize,
    halfScreenSize,
    directionToCoordinate
) where

import Constants

{- Data structures -}
data Coordinate = Coordinate Float Float deriving (Eq, Ord, Show)

data Direction = DNone | DUp | DDown | DLeft | DRight deriving (Eq, Ord, Show)

{- Classes -}
instance Num Coordinate where
    (+) (Coordinate x1 y1) (Coordinate x2 y2) = Coordinate (x1 + x2) (y1 + y2)
    (-) (Coordinate x1 y1) (Coordinate x2 y2) = Coordinate (x1 - x2) (y1 - y2)
    (*) (Coordinate x1 y1) (Coordinate x2 y2) = Coordinate (x1 * x2) (y1 * y2)
    abs (Coordinate x y) = Coordinate (abs x) (abs y)
    signum (Coordinate x y) = Coordinate (signum x) (signum y)
    fromInteger i = Coordinate (fromInteger i) (fromInteger i)

instance Fractional Coordinate where
    (/) (Coordinate x1 y1) (Coordinate x2 y2) = Coordinate (x1 / x2) (y1 / y2)
    recip (Coordinate x y) = Coordinate (recip x) (recip y)
    fromRational r = Coordinate (fromRational r) (fromRational r)

{- Functions -}
fromFloat :: Float -> Coordinate
fromFloat f = Coordinate f f

screenSize = Coordinate (fromIntegral width) (fromIntegral height)
halfScreenSize = screenSize / 2

directionToCoordinate :: Direction -> Coordinate
directionToCoordinate direction = case direction of DNone  -> Coordinate 0    0
                                                    DUp    -> Coordinate 0    (-1)
                                                    DDown  -> Coordinate 0    1
                                                    DLeft  -> Coordinate (-1) 0
                                                    DRight -> Coordinate 1    0