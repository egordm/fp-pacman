module Game.Input (
    InputType(..),
    InputData(..)
) where
    
import Graphics.Gloss.Game(Key(..))
import Engine.Core.Coordinate

    
{- Data structures -}
data InputType = Keyboard [(Key, Direction)] deriving (Eq, Show)

data InputData = InputData InputType Direction deriving (Eq, Show)

{- Classes -}


{- Instances -}


{- Functions -}

