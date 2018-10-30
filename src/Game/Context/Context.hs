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
import Resources

{- Data structures -}

data Context = Context {
    room :: Room,
    roomName :: String,
    rooms :: Map.Map String Room,
    sounds :: Sounds
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
makeContext :: RoomCollection -> Sounds -> Context
makeContext (RoomCollection first@(name,start) others) sounds = Context start name roomMap sounds
    where roomMap = Map.fromList $ first:others

-- IO Wrapping
inputIO :: Event -> Context -> IO Context
inputIO e c = return (input e c)

renderIO :: Context -> IO Picture
renderIO c = return (render c)

updateIO :: Float -> Context -> IO Context
updateIO dt c = do
                  let bc = baseUpdate dt c
                  return bc

playContext playFn context@Context{room,rooms} = playFn context renderIO inputIO updateIO