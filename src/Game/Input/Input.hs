module Game.Input.Input (
    InputType(..),
    InputData(..),
    Inputable(..),
    arrowInput,
    wasdInput
) where

import Prelude hiding (Left, Right)
import Graphics.Gloss.Game
import Engine.Core.Base
import Game.Input.Classes

    
{- Data structures -}
data InputType = Keyboard [(Key, Direction)] deriving (Eq, Show)

data InputData = InputData InputType Direction deriving (Eq, Show)

{- Classes -}


{- Instances -}
instance Inputable InputData where -- TODO: can be done better by storing all pressed kays and the making one from them
    input (EventKey key state _ _) (InputData inputType@(Keyboard keyMapping) currentDirection)
        = InputData inputType newDirection
          where matchDirection = matchKey key keyMapping
                newDirection   = updateDirection state currentDirection matchDirection
    input _ inputData = inputData

{- Functions -}
-- | Matches key to a direction in given mapping
matchKey :: Key -> [(Key, Direction)] -> Direction
matchKey _   [] = DNone
matchKey key ((mappingKey, direction):ms) | key == mappingKey = direction
                                          | otherwise         = matchKey key ms

-- | Updates direction given input direction, input state and current direction
updateDirection :: KeyState -> Direction -> Direction -> Direction
updateDirection Graphics.Gloss.Game.Down _ inputDirection = inputDirection
updateDirection Graphics.Gloss.Game.Up currentDirection inputDirection | currentDirection == inputDirection = DNone
                                                                       | otherwise = currentDirection

{- Constants -}
arrowInput, wasdInput :: InputData
arrowInput = InputData (Keyboard [(SpecialKey KeyUp, DUp), (SpecialKey KeyDown, DDown), (SpecialKey KeyLeft, DLeft), (SpecialKey KeyRight, DRight)]) DNone

wasdInput = InputData (Keyboard [(Char 'w', DUp), (Char 's', DDown), (Char 'a', DLeft), (Char 'd', DRight)]) DNone
