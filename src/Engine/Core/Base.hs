module Engine.Core.Base (
    Updateable(..),

    Coordinate(..),
    coord,
    coordf,
    coordScreenS,
    coordScreenSH,
    coordZ,
    coordToPos,
    coordDotp,
    coordDist,
    posToCoord,

    Pos(..),
    posZ,

    Direction(..),
    dirs,
    dirOrh,
    dirOpposite,
    dirOpposite,
    coordComp,
    dirToPos,
    dirToCoord,
    isDirsOrth

) where
    
import Engine.Core.Classes
import Engine.Core.Coordinate
import Engine.Core.Direction
import Engine.Core.Position