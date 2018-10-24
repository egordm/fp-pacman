{-# LANGUAGE NamedFieldPuns #-}

module Game.Room(
    Room(..),
    RoomInputFunc,
    RoomRenderFunc,
    RoomUpdateFunc,
    RoomFunctions,
    makeRoom,
    Context(..),
    ContextInputFunc,
    ContextRenderFunc,
    ContextUpdateFunc,
    ContextFunctions,
    stdConFuncs,
    makeContext,
    RoomCollection(..),
    RoomEntry
) where

import Graphics.Gloss(Picture)
import Graphics.Gloss.Game
import Game.World
import Game.GameState
import Game.Level
import Game.Internal
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
    rooms :: Map.Map String Room,
    cInput :: ContextInputFunc,
    cRender :: ContextRenderFunc,
    cUpdate :: ContextUpdateFunc
}

type ContextInputFunc = (Event -> Context -> Context)
type ContextRenderFunc = (Context -> Picture)
type ContextUpdateFunc = (Float -> Context -> Context)
type ContextFunctions = (ContextInputFunc, ContextRenderFunc, ContextUpdateFunc)

type RoomEntry = (String, Room)
data RoomCollection = RoomCollection RoomEntry [RoomEntry]

{- Functions -}

stdConInput :: ContextInputFunc
stdConInput e c@Context{room = cr} = 
    c{room = newRoom cr}
    where
        newRoom r@Room{state = cs,rInput = rfi} = 
            r{state = rfi e cs}

stdConRender :: ContextRenderFunc
stdConRender Context{room} = 
    pictureFromRoom room
    where
        pictureFromRoom Room{state,rRender} = 
            rRender state

--TODO, here we can switch to a context with a new room and its functions
stdConUpdate :: ContextUpdateFunc
stdConUpdate time c@Context{room = cr, rooms = rm, roomName = crm} = 
    case cr of
        Room{state = GameState{switch = ContextStay}} -> nextContext
        Room{state = GameState{switch = (ContextSwitch req)}} -> newContext req
    where
        nextContext = c{room = nextRoom cr}
        nextRoom r@Room{state = cs,rUpdate = rfu} = 
            r{state = rfu time cs}
        newContext name = 
            case (Map.lookup name rm) of
            Nothing -> nextContext
            Just foundRoom@Room{state = foundState} ->
                let newRooms = Map.insert crm cr rm in
                c{roomName = name, rooms = newRooms, room = foundRoom{state = foundState{t = 0, switch = ContextStay}}}

stdConFuncs :: ContextFunctions
stdConFuncs = (stdConInput, stdConRender, stdConUpdate)

makeContext :: RoomCollection -> Context
makeContext (RoomCollection first@(name,start) others) = Context start name roomMap stdConInput stdConRender stdConUpdate
    where
        roomMap = Map.fromList $ first:others

makeRoom :: GameState -> RoomFunctions -> Room
makeRoom istate (i,r,u) = Room istate istate i r u