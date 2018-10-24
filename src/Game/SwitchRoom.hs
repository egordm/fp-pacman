module Game.SwitchRoom(
    SwitchRoomMode(..),
    SwitchRoom(..)
) where

{- Data structures -}

data SwitchRoomMode = ResumeRoom | ReloadRoom deriving(Show)
data SwitchRoom = RoomStay | RoomSwitch String SwitchRoomMode deriving(Show)