{-# LANGUAGE NamedFieldPuns #-}

module Game.Context.Context(
    Context,
    RoomCollection(..),
    RoomEntry,
    makeContext,
    playContext
) where

import Graphics.Gloss(Picture)
import Graphics.Gloss.Game
import Engine.Base
import Game.Context.SwitchRoom
import Game.Context.Room
import Game.Structure.GameState
import Game.Rules.Base
import Game.Input.Base
import qualified Data.Map as Map
import Control.Arrow

{- Data structures -}

data Context = Context {
    room :: Room,
    roomName :: String,
    rooms :: Map.Map String Room
}

type RoomEntry = (String, Room)
data RoomCollection = RoomCollection RoomEntry [RoomEntry]

{- Instances -}
instance Inputable Context where
    input e c@Context{room=r} = c{room=input e r}

instance Renderable Context where
    render c@Context{room} = render room

instance BaseUpdateable Context where
    baseUpdate dt c@Context{room=r, rooms, roomName} -- TODO: Function still does too much
        = case r of
            Room{state = GameState{switch = RoomStay}} -> c{room = baseUpdate dt r}
            Room{state = GameState{switch = RoomReload}} -> roomAction roomName ReloadRoom
            Room{state = GameState{switch = (RoomSwitch req mode)}} -> roomAction req mode
          where
              roomAction newRoomName mode = case (Map.lookup newRoomName rooms) of
                  Nothing -> c{room = baseUpdate dt r}
                  Just nr@Room{state, initState} ->
                      let nrooms = Map.insert roomName r rooms
                          nstate = case mode of
                              ResumeRoom -> state
                              ReloadRoom -> initState
                      in c{roomName = newRoomName, rooms = nrooms, room = nr{state = nstate{t = 0, switch = RoomStay}}}


{- Functions -}
makeContext :: RoomCollection -> Context
makeContext (RoomCollection first@(name,start) others) = Context start name roomMap
    where
        roomMap = Map.fromList $ first:others

playContext f context@Context{room,rooms} = 
    f context render input [baseUpdate]