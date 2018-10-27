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
import Game.Context.SwitchRoom
import Game.Context.Room
import Game.Structure.GameState
import Game.Rules.Base
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

stdConUpdate :: Float -> Context -> Context
stdConUpdate time c@Context{room = cr, rooms = rm, roomName = crm} = 
    case cr of
        Room{state = GameState{switch = RoomStay}} -> nextContext
        Room{state = GameState{switch = RoomReload}} -> newContext crm ReloadRoom
        Room{state = GameState{switch = (RoomSwitch req mode)}} -> newContext req mode
    where
        nextContext = c{room = nextRoom cr}
        nextRoom r@Room{state = cs,rUpdate = rfu,rules = rls} = 
            r{state = applyRules rls $ rfu time cs}
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

playContext f context@Context{room,rooms} = 
    f context stdConRender stdConInput [stdConUpdate]