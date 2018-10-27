module Game.Input.Classes (
    Inputable(..)
) where

import Graphics.Gloss.Game(Event(..))

{- Classes -}
class Inputable a where
    input :: Event -> a -> a

