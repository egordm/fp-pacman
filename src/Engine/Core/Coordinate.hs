module Engine.Core.Coordinate where

import Engine.Core.Position
import Constants

{- Data structures -}
-- | Coordinate consistent of 2 floating points
data Coordinate = Coordinate Float Float deriving (Eq, Ord, Show)

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
-- | Coordinate quick constructor
coord :: Int -> Int -> Coordinate
coord x y = Coordinate (realToFrac x) (realToFrac y)

-- | Create coordinate from float
coordf :: Float -> Coordinate
coordf f = Coordinate f f

-- | Screen sizes
coordScreenS, coordScreenSH, coordZ :: Coordinate
coordScreenS = Coordinate (fromIntegral width) (fromIntegral height)
coordScreenSH = coordScreenS / 2

-- | Zero coordinate
coordZ = Coordinate 0 0

-- | Coordinate to position
coordToPos :: Coordinate -> Pos
coordToPos (Coordinate x y) = Pos (round x) (round y)

-- | Position to coordinate
posToCoord :: Pos -> Coordinate
posToCoord (Pos x y) = Coordinate (realToFrac x) (realToFrac y)

-- | Dot product of a coordinate with itself
coordDotp :: Coordinate -> Float
coordDotp (Coordinate x y) = x*x + y*y

-- | Pytagorean Distance between two coordinates
coordDist :: Coordinate -> Coordinate -> Float
coordDist a b = sqrt (coordDotp (a - b))

