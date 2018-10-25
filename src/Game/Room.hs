{-# LANGUAGE NamedFieldPuns #-}

module Game.Room(
    Room,
    RoomInputFunc,
    RoomRenderFunc,
    RoomUpdateFunc,
    RoomFunctions,
    makeRoom,
    playRoom,
    Context,
    makeContext,
    playContext,
    RoomCollection(..),
    RoomEntry
) where

import Graphics.Gloss(Picture)
import Graphics.Gloss.Game
import Game.World
import Game.GameState
import Game.Level
import Game.SwitchRoom
import qualified Data.Map as Map

{- Data structures -}

data Room = Room {
    state :: GameState,
    initState :: GameState,
    rInput :: RoomInputFunc,
    rRender :: RoomRenderFunc,
    rUpdate :: RoomUpdateFunc
}

type RoomInputFunc = (Event -> GameState -> GameState)
type RoomRenderFunc = (GameState -> Picture)
type RoomUpdateFunc = (Float -> GameState -> GameState)
type RoomFunctions = (RoomInputFunc, RoomRenderFunc, RoomUpdateFunc)

data Context = Context {
    room :: Room,
    roomName :: String,
    rooms :: Map.Map String Room
}

type RoomEntry = (String, Room)
data RoomCollection = RoomCollection RoomEntry [RoomEntry]

{- Functions -}

stdConInput :: Event -> Context -> Context
stdConInput e c@Context{room = cr} = 
    c{room = newRoom cr}
    where
        newRoom r@Room{state = cs,rInput = rfi} = 
            r{state = rfi e cs}

stdConRender :: Context -> Picture
stdConRender Context{room} = 
    pictureFromRoom room
    where
        pictureFromRoom Room{state,rRender} = 
            rRender state

--TODO, here we can switch to a context with a new room and its functions
stdConUpdate :: Float -> Context -> Context
stdConUpdate time c@Context{room = cr, rooms = rm, roomName = crm} = 
    case cr of
        Room{state = GameState{switch = RoomStay}} -> nextContext
        Room{state = GameState{switch = (RoomSwitch req mode)}} -> newContext req mode
    where
        nextContext = c{room = nextRoom cr}
        nextRoom r@Room{state = cs,rUpdate = rfu} = 
            r{state = rfu time cs}
        newContext name mode = 
            case (Map.lookup name rm) of
            Nothing -> nextContext
            Just foundRoom@Room{state = foundState, initState = foundInit} ->
                let 
                    newRooms = Map.insert crm cr rm 
                    newState = case mode of
                        ResumeRoom -> foundState
                        ReloadRoom -> foundInit
                    in
                c{roomName = name, rooms = newRooms, room = foundRoom{state = newState{t = 0, switch = RoomStay}}}

makeContext :: RoomCollection -> Context
makeContext (RoomCollection first@(name,start) others) = Context start name roomMap
    where
        roomMap = Map.fromList $ first:others

makeRoom :: GameState -> RoomFunctions -> Room
makeRoom istate (i,r,u) = Room istate istate i r u

playContext f context@Context{room,rooms} = 
    f context stdConRender stdConInput [stdConUpdate]

playRoom f Room{ initState, rRender, rInput, rUpdate } = 
    f initState rRender rInput [rUpdate]