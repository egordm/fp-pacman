{-# LANGUAGE NamedFieldPuns #-}

module Game.Room(
    Room(..),
    RoomInputFunc,
    RoomRenderFunc,
    RoomUpdateFunc,
    RoomFunctions,
    makeRoom
) where

import Graphics.Gloss(Picture)
import Graphics.Gloss.Game
import Game.World
import Game.GameState
import Game.Level

{- Data structures -}

data Room = Room {
    state :: GameState,
    initState :: GameState,
    fInput :: RoomInputFunc,
    fRender :: RoomRenderFunc,
    fUpdate :: RoomUpdateFunc
}

type RoomInputFunc = (Event -> GameState -> GameState)
type RoomRenderFunc = (GameState -> Picture)
type RoomUpdateFunc = (Float -> GameState -> GameState)
type RoomFunctions = (RoomInputFunc, RoomRenderFunc, RoomUpdateFunc)

{- Functions -}

makeRoom :: GameState -> RoomFunctions -> Room
makeRoom istate (i,r,u) = Room istate istate i r u