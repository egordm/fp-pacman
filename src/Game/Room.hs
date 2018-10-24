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
    makeContext
) where

import Graphics.Gloss(Picture)
import Graphics.Gloss.Game
import Game.World
import Game.GameState
import Game.Level
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
    initRoom :: Room,
    cInput :: ContextInputFunc,
    cRender :: ContextRenderFunc,
    cUpdate :: ContextUpdateFunc
}

type ContextInputFunc = (Event -> Context -> Context)
type ContextRenderFunc = (Context -> Picture)
type ContextUpdateFunc = (Float -> Context -> Context)
type ContextFunctions = (ContextInputFunc, ContextRenderFunc, ContextUpdateFunc)

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
stdConUpdate time c@Context{room = cr} = 
    c{room = newRoom cr}
    where
        newRoom r@Room{state = cs,rUpdate = rfu} = 
            r{state = rfu time cs}

stdConFuncs :: ContextFunctions
stdConFuncs = (stdConInput, stdConRender, stdConUpdate)

makeContext :: Room -> Context
makeContext iroom = Context iroom iroom stdConInput stdConRender stdConUpdate

makeRoom :: GameState -> RoomFunctions -> Room
makeRoom istate (i,r,u) = Room istate istate i r u