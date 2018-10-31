module Game.UI.MenuItem (
    MenuItem(..), MenuItem_(..)
) where

import Game.UI.Text
import Game.Context.SwitchRoom
import Engine.Base
import Graphics.Gloss.Game(Event(..))

class MenuItem_ a where
    decide :: a -> SwitchRoom
    updateItem :: Int -> a -> a
    drawItem :: a -> [DrawInstruction]
    inputItem :: Event -> a -> a
    soundItem :: a -> [SoundInstruction]

data MenuItem = Label {msg :: FontString}

instance MenuItem_ MenuItem where
    decide mi@Label{msg} = RoomStay

    updateItem i mi@Label{msg} = mi

    drawItem mi@Label{msg} = draw msg

    inputItem e mi@Label{msg} = mi

    soundItem mi@Label{msg} = []