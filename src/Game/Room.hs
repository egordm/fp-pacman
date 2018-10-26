{-# LANGUAGE NamedFieldPuns #-}

module Game.Room(
    Room(..),
    RoomInputFunc,
    RoomRenderFunc,
    RoomUpdateFunc,
    RoomFunctions,
    makeRoom,
    playRoom
) where

import Graphics.Gloss(Picture)
import Graphics.Gloss.Game
import Game.GameState
import Game.Rule

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
{- Functions -}

makeRoom :: GameState -> RoomFunctions -> Room
makeRoom istate (i,r,u) = Room istate istate i r u

playRoom f Room{ initState, rRender, rInput, rUpdate } = 
    f initState rRender rInput [rUpdate]