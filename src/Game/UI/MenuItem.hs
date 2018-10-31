module Game.UI.MenuItem (
    MenuItem(..), MenuItem_(..),
    Anchor(..),
    makeLabel
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

data Anchor = TopLeft | Center

data MenuItem = Label {msg :: FontString}

instance MenuItem_ MenuItem where
    decide mi@Label{msg} = RoomStay

    updateItem i mi@Label{msg} = mi

    drawItem mi@Label{msg} = draw msg

    inputItem e mi@Label{msg} = mi

    soundItem mi@Label{msg} = []

calcPosWithAnchor :: String -> Coordinate -> Anchor -> Coordinate
calcPosWithAnchor msg pos TopLeft = pos
calcPosWithAnchor msg pos Center = finalPos pos dimensions
    where
        dimensions = textDimensions msg
        finalPos (Coordinate x y) (w, h) = Coordinate (x - w/2) (y - h/2)

makeLabel :: String -> Coordinate -> Anchor -> MenuItem
makeLabel msg pos anchor = Label $ FontString msg finalPos
    where
        finalPos = calcPosWithAnchor msg pos anchor
