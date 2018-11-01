module Game.Context.Room(
    Room(..),
    RoomInputFunc,
    RoomUpdateFunc,
    RoomFunctions,
    makeRoom,
    makeMenu,
    playRoom,
    applyRules
) where

import Graphics.Gloss(Picture)
import Graphics.Gloss.Game
import Engine.Base
import Game.Structure.GameState
import Game.Rules.Rules
import Game.UI.Base
import Game.Context.SwitchRoom

{- Data structures -}
data Room = Room {
    state :: GameState,
    initState :: GameState,
    rules :: [GameRule],
    rInput :: RoomInputFunc,
    rUpdate :: RoomUpdateFunc
} | Menu {
    menuState :: [MenuItem],
    initMenu :: [MenuItem],
    menuSwitch :: SwitchRoom
}

type RoomInputFunc = (Event -> GameState -> GameState)
type RoomRenderFunc = (GameState -> Picture)
type RoomUpdateFunc = (Float -> GameState -> GameState)
type RoomFunctions = (RoomInputFunc, RoomUpdateFunc)

{- Instances -}
instance Inputable Room where
    input e r@Room{state, rInput} = r{state=nstate} where nstate = rInput e state
    input e m@Menu{menuState} = m{menuState=nstate} where nstate = map (inputItem e) menuState

instance Renderable Room where
    render r@Room{state} = renderInstructions $ draw state
    render m@Menu{menuState} = renderInstructions $ concatMap drawItem menuState

instance BaseUpdateable Room where
    baseUpdate dt r@Room{rules, rUpdate, state} = r{state=nstate} where nstate = applyRules rules $ rUpdate dt state
    baseUpdate dt m@Menu{menuState} = m{menuState = nstate, menuSwitch = nswitch nstate} 
        where 
            nstate = map (updateItem 0) menuState
            nswitch [] = RoomStay
            nswitch (x:xs) = case (decide x) of
                RoomStay -> nswitch xs
                other -> other

instance Soundable Room where
    doSound Room{state} = doSound state
    doSound Menu{menuState} = concatMap soundItem menuState

{- Functions -}
makeRoom :: GameState -> [GameRule] -> RoomFunctions -> Room
makeRoom istate rules (i,u) = Room istate istate rules i u

makeMenu :: [MenuItem] -> Room
makeMenu items = Menu items items RoomStay

playRoom f Room{ initState, rInput, rUpdate } =
    f initState render rInput [rUpdate]