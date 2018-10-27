{-# LANGUAGE NamedFieldPuns #-}

module Game.Room(
    Room(..),
    RoomInputFunc,
    RoomRenderFunc,
    RoomUpdateFunc,
    RoomFunctions,
    makeRoom,
    playRoom,
    applyRules
) where

import Graphics.Gloss(Picture)
import Graphics.Gloss.Game
import Game.GameState
import Game.Rule
import Control.Arrow

{- Data structures -}

data Room = Room {
    state :: GameState,
    initState :: GameState,
    rules :: [Rule],
    rInput :: RoomInputFunc,
    rRender :: RoomRenderFunc,
    rUpdate :: RoomUpdateFunc
}

type RoomInputFunc = (Event -> GameState -> GameState)
type RoomRenderFunc = (GameState -> Picture)
type RoomUpdateFunc = (Float -> GameState -> GameState)
type RoomFunctions = (RoomInputFunc, RoomRenderFunc, RoomUpdateFunc)

{- Functions -}

applyRules :: [Rule] -> GameState -> GameState
applyRules rls st = (foldl (>>>) id rls) st

makeRoom :: GameState -> [Rule] -> RoomFunctions -> Room
makeRoom istate rules (i,r,u) = Room istate istate rules i r u

playRoom f Room{ initState, rRender, rInput, rUpdate } =
    f initState rRender rInput [rUpdate]