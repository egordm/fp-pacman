module Game.Context.Room(
    Room(..),
    RoomInputFunc,
    RoomUpdateFunc,
    RoomFunctions,
    makeRoom,
    makeMenu,
    playRoom
) where

import Graphics.Gloss(Picture)
import Graphics.Gloss.Game
import Engine.Base
import Game.Structure.GameState
import Game.Structure.MenuState
import Game.Rules.Rules
import Game.UI.Base
import Game.Context.SwitchRoom

{- Data structures -}
data Room = Room {
    state :: GameState,
    initState :: GameState,
    gameRules :: [GameRule],
    gameInputRules :: [GameInputRule],
    rUpdate :: RoomUpdateFunc
} | Menu {
    menuState :: MenuState,
    initMenu :: MenuState,
    menuSwitch :: SwitchRoom
}

type RoomInputFunc = (Event -> GameState -> GameState)
type RoomRenderFunc = (GameState -> Picture)
type RoomUpdateFunc = (Float -> GameState -> GameState)
type RoomFunctions = (RoomInputFunc, RoomUpdateFunc)

{- Instances -}
instance Inputable Room where
    input e r@Room{state, gameInputRules} = r{state=nstate} where nstate = applyGameInputRules e gameInputRules state
    input e m@Menu{menuState=ms} = m{menuState=ms{items = map (inputItem e) (items ms)}}

instance Renderable Room where
    render r@Room{state} = renderInstructions $ draw state
    render m@Menu{menuState} = renderInstructions $ concatMap drawItem $ items menuState

instance BaseUpdateable Room where
    baseUpdate dt r@Room{gameRules, rUpdate, state} = r{state=nstate} where nstate = applyGameRules gameRules $ rUpdate dt state
    baseUpdate dt m@Menu{menuState=ms} = m{menuState = nstate, menuSwitch = nswitch (items nstate)} 
        where
            nstate = ms{items = map (updateItem 0) (items ms)}
            nswitch [] = RoomStay
            nswitch (x:xs) = case (decide x) of
                RoomStay -> nswitch xs
                other -> other

instance Soundable Room where
    doSound Room{state} = doSound state
    doSound Menu{menuState} = concatMap soundItem $ items menuState

{- Functions -}
makeRoom :: GameState -> [GameRule] -> [GameInputRule] -> RoomUpdateFunc -> Room
makeRoom istate gameRules inputRules u = Room istate istate gameRules inputRules u

makeMenu :: [MenuItem] -> Room
makeMenu items = Menu startState startState RoomStay
    where
        startState = MenuState items 0

playRoom f Room{ initState, rUpdate } =
    f initState render input [rUpdate]