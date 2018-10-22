module Engine.Core.Coordinate (
    Coordinate(..),
    Direction(..),
    screenSize,
    halfScreenSize
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
screenSize = Coordinate (fromIntegral width) (fromIntegral height)
halfScreenSize = screenSize / 2