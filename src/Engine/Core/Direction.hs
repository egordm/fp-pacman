module Engine.Core.Direction where
    
import Engine.Core.Position
import Engine.Core.Coordinate
import Constants


{- Data structures -}
data Direction = DNone | DUp | DDown | DLeft | DRight deriving (Eq, Ord, Show)


{- Classes -}


{- Instances -}


{- Functions -}
dirs :: [Direction]
dirs = [DNone, DUp, DDown, DLeft, DRight]


dirOrh, dirOpposite :: Direction -> Direction
dirOrh d = case d of
    DNone   -> DNone
    DUp     -> DLeft
    DDown   -> DLeft
    DLeft   -> DUp
    DRight  -> DUp

dirOpposite d = case d of
    DNone   -> DNone
    DUp     -> DDown
    DDown   -> DUp
    DLeft   -> DRight
    DRight  -> DLeft

coordComp :: Direction -> Coordinate -> Coordinate
coordComp d (Coordinate x y) = case d of
    DNone  -> coordZ
    DUp    -> Coordinate 0 y
    DDown  -> Coordinate 0 y
    DLeft  -> Coordinate x 0
    DRight -> Coordinate x 0

dirToPos :: Direction -> Pos
dirToPos direction = case direction of
    DNone  -> Pos 0    0
    DUp    -> Pos 0    (-1)
    DDown  -> Pos 0    1
    DLeft  -> Pos (-1) 0
    DRight -> Pos 1    0

dirToCoord :: Direction -> Coordinate
dirToCoord = posToCoord . dirToPos

isDirsOrth :: Direction -> Direction -> Bool
isDirsOrth d1 d2 = coordDotp (dirToCoord d1 * dirToCoord d2) < epsilon