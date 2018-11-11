module Game.UI.MenuItem (
    MenuItem(..), MenuItem_(..),
    Anchor(..),
    makeLabel, makeLabelF, makeButton
) where

import Game.UI.Text
import Game.Context.SwitchRoom
import Engine.Base
import Graphics.Gloss.Game(Event(..))
import Game.Context.Persistant

class MenuItem_ a where
    decide :: a -> SwitchRoom
    updateItem :: Int -> Persistant -> Persistant -> a -> a
    drawItem :: a -> [DrawInstruction]
    inputItem :: Event -> a -> a
    soundItem :: a -> [SoundInstruction]

data Anchor = TopLeft | Center

data MenuItem = 
    Label {
        msg :: FontString,
        labelPos :: Coordinate,
        labelUpdate :: (MenuItem -> Persistant -> Persistant -> MenuItem)
    } |
    Button {
        buttonPos :: Coordinate,
        normalMsg :: String, 
        selectedMsg :: String, 
        msg :: FontString, 
        nr :: Int, 
        isSelected :: Bool, 
        itemSwitch :: SwitchRoom, 
        inputF :: (Event -> MenuItem -> MenuItem)
    }

instance MenuItem_ MenuItem where
    decide mi@Label{msg} = RoomStay
    decide mi@Button{itemSwitch} = itemSwitch

    updateItem i oldPD newPD mi@Label{msg, labelUpdate} = labelUpdate mi oldPD newPD
    updateItem i _ _ mi@Button{nr, buttonPos = pos, normalMsg, selectedMsg}
        = mi{isSelected = selected, msg = nmsg selected}
        where 
            selected = (i == nr)
            nmsg True = msg $ makeLabel selectedMsg pos Center
            nmsg False = msg $ makeLabel normalMsg pos Center
    
    drawItem mi@Label{msg} = draw msg
    drawItem mi@Button{msg} = draw msg
    
    inputItem e mi@Label{msg} = mi
    inputItem e mi@Button{inputF = f, isSelected = True} = f e mi
    inputItem e mi = mi

    soundItem mi@Label{msg} = []
    soundItem mi@Button{msg} = []

calcPosWithAnchor :: String -> Coordinate -> Anchor -> Coordinate
calcPosWithAnchor msg pos TopLeft = pos
calcPosWithAnchor msg pos Center = finalPos pos dimensions
    where
        dimensions = textDimensions msg
        finalPos (Coordinate x y) (w, h) = Coordinate (fromIntegral $ floor(x - (w-1)/2)) (fromIntegral $ floor(y - (h-1)/2))

makeLabel :: String -> Coordinate -> Anchor -> MenuItem
makeLabel msg pos anchor = Label (FontString msg finalPos) pos (\l _ _ -> l)
    where
        finalPos = calcPosWithAnchor msg pos anchor

makeLabelF :: String -> Coordinate -> Anchor -> (MenuItem -> Persistant -> Persistant -> MenuItem) -> MenuItem
makeLabelF msg pos anchor f = Label (FontString msg finalPos)  pos f
    where
        finalPos = calcPosWithAnchor msg pos anchor

makeButton :: String -> String -> Int -> (Event -> MenuItem -> MenuItem) -> Coordinate -> MenuItem
makeButton normalMsg selectedMsg nr func pos
    = Button pos normalMsg selectedMsg fmsg nr False RoomStay func
    where fmsg = msg $ makeLabel normalMsg pos Center