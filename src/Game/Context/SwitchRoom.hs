module Game.Context.SwitchRoom(
    SwitchRoomMode(..),
    SwitchRoom(..)
) where

{- Data structures -}
--this data is used to let the context know when to switch and how to switch room
data SwitchRoomMode = ResumeRoom | ReloadRoom deriving(Show)
data SwitchRoom = RoomStay | RoomReload | RoomSwitch String SwitchRoomMode deriving(Show)