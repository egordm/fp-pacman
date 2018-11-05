module Engine.Core.Position where
    

{- Data structures -}
-- | Position consistent of 2 integer points
data Pos = Pos Int Int deriving (Eq, Ord, Show)


{- Classes -}


{- Instances -}
instance Num Pos where
    (+) (Pos x1 y1) (Pos x2 y2) = Pos (x1 + x2) (y1 + y2)
    (-) (Pos x1 y1) (Pos x2 y2) = Pos (x1 - x2) (y1 - y2)
    (*) (Pos x1 y1) (Pos x2 y2) = Pos (x1 * x2) (y1 * y2)
    abs (Pos x y) = Pos (abs x) (abs y)
    signum (Pos x y) = Pos (signum x) (signum y)
    fromInteger i = Pos (fromInteger i) (fromInteger i)


{- Functions -}
posZ :: Pos
posZ = Pos 0 0

