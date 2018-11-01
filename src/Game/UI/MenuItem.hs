module Game.UI.MenuItem (
    MenuItem(..), MenuItem_(..),
    Anchor(..),
    makeLabel, makeButton
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

data MenuItem = 
    Label {msg :: FontString} |
    Button {normalMsg :: String, selectedMsg :: String, msg :: FontString, nr :: Int, 
            isSelected :: Bool, itemSwitch :: SwitchRoom, inputF :: (Event -> MenuItem -> MenuItem)}

instance MenuItem_ MenuItem where
    decide mi@Label{msg} = RoomStay
    decide mi@Button{itemSwitch} = itemSwitch

    updateItem i mi@Label{msg} = mi
    updateItem i mi@Button{nr, msg = omsg, normalMsg, selectedMsg}
        = mi{isSelected = selected, msg = nmsg selected omsg}
        where 
            selected = (i == nr)
            nmsg True (FontString str coor) = FontString selectedMsg coor
            nmsg False (FontString str coor) = FontString normalMsg coor
    
    drawItem mi@Label{msg} = draw msg
    drawItem mi@Button{msg} = draw msg

    inputItem e mi@Label{msg} = mi
    inputItem e mi@Button{inputF = f} = f e mi 

    soundItem mi@Label{msg} = []
    soundItem mi@Button{msg} = []

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

makeButton :: String -> String -> Int -> (Event -> MenuItem -> MenuItem) -> Coordinate -> MenuItem
makeButton normalMsg selectedMsg nr func pos
    = Button normalMsg selectedMsg msg nr False RoomStay func
    where msg = FontString normalMsg (calcPosWithAnchor normalMsg pos Center)
