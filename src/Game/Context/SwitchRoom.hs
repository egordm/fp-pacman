module Game.Context.SwitchRoom(
    SwitchRoomMode(..),
    SwitchRoom(..)
) where

{- Data structures -}

data SwitchRoomMode = ResumeRoom | ReloadRoom deriving(Show)
data SwitchRoom = RoomStay | RoomReload | RoomSwitch String SwitchRoomMode deriving(Show)