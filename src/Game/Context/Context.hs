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
import Game.Structure.MenuState
import Game.Rules.Base
import Game.Input.Base
import qualified Data.Map as Map
import Resources
import Game.Context.Persistant

{- Data structures -}
--Context: way to switch rooms, like menu's and levels
data Context = Context {
    room :: Room,
    roomName :: String,
    rooms :: Map.Map String Room,
    sounds :: Sounds
}
--A room and its name
type RoomEntry = (String, Room)
--Always have a valid room to start
data RoomCollection = RoomCollection RoomEntry [RoomEntry]

{- Instances -}
--do input
instance Inputable Context where
    input e c@Context{room=r} = c{room=input e r}
--do rendering
instance Renderable Context where
    render c@Context{room} = render room
--update and check if we need to switch rooms
instance BaseUpdateable Context where
    baseUpdate dt c@Context{room=r, rooms, roomName}
        = case (extractSwitch r) of
            RoomStay -> c{room = baseUpdate dt r}
            RoomReload -> roomAction roomName ReloadRoom
            (RoomSwitch req mode) -> roomAction req mode
        where
            roomAction nn mo = newContext c rooms r roomName nn mo dt
--get the switch object from a room
extractSwitch :: Room -> SwitchRoom
extractSwitch Room{state = GameState{switch = s}} = s
extractSwitch Menu{menuSwitch = s} = s
--look a room up in the map
findRoom :: String -> Map.Map String Room -> Maybe Room
findRoom name rooms = Map.lookup name rooms
--add a new room
insertRoom :: String -> Room -> Map.Map String Room -> Map.Map String Room
insertRoom name room rooms = Map.insert name room rooms
--switch to the new room, resetting it properly
switchTo :: Room -> SwitchRoomMode -> Room
switchTo r@Room{state = st} ResumeRoom = r{state = st{t = 0, switch = RoomStay}}
switchTo r@Room{initState = st} ReloadRoom = r{state = st{t = 0, switch = RoomStay}}
switchTo m@Menu{menuState = st} ResumeRoom = m{menuSwitch = RoomStay}
switchTo m@Menu{initMenu = st} ReloadRoom = m{menuSwitch = RoomStay, menuState = st}
--transfer the persistant data between rooms
transferPersistant :: Room -> Room -> Room
transferPersistant r@Room{state = os} newroom@Room{state = ns} = 
    newroom{state = ns{gameOldPersistant = gameNewPersistant os}}
transferPersistant r@Room{state = os} newroom@Menu{menuState = ns} = 
    newroom{menuState = ns{menuOldPersistant = gameNewPersistant os}}
transferPersistant r@Menu{menuState = os} newroom@Room{state = ns} = 
    newroom{state = ns{gameOldPersistant = menuNewPersistant os}}
transferPersistant r@Menu{menuState = os} newroom@Menu{menuState = ns} = 
    newroom{menuState = ns{menuOldPersistant = menuNewPersistant os}}
-- end the sound on switch
killSound :: Room -> Room
killSound r@Room{state = st} = r{state = st{bgSound = None}}
killSound r = r
--here the switching comes together
newContext oldContext oldRooms oldRoom oldRoomName newRoomName mode dt = case (findRoom newRoomName oldRooms) of
    Nothing -> oldContext{room = baseUpdate dt oldRoom}
    Just found ->
        let nrooms = insertRoom oldRoomName oldRoom oldRooms
            nroom = resetTick $ killSound $ transferPersistant oldRoom $ switchTo found mode
        in oldContext{roomName = newRoomName, rooms = nrooms, room = nroom}
--get the sound instructions
instance Soundable Context where
    doSound Context{room} = doSound room

{- Functions -}
--constructor to build a context
makeContext :: RoomCollection -> Sounds -> Context
makeContext (RoomCollection first@(name,start) others) sounds = Context start name roomMap sounds
    where roomMap = Map.fromList $ first:others

-- IO Wrapping
inputIO :: Event -> Context -> IO Context
inputIO e c = return (input e c)
--render
renderIO :: Context -> IO Picture
renderIO c = return (render c)
--update, and do all IO stuff, like file stuff and sound
updateIO :: Float -> Context -> IO Context
updateIO dt c = do
    let bc = baseUpdate dt c
    playSoundInstructions (sounds bc) (doSound bc)
    newr <- applyIOF $ room bc
    if isFirstTick (room bc)
    then return bc{room = newr}
    else return bc
--play the damn thing
playContext playFn context@Context{room,rooms} = playFn context renderIO inputIO updateIO