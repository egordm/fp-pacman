module Game.Context.Room(
    Room(..),
    RoomUpdateFunc,
    makeRoom,
    makeMenu,
    makeMenuF,
    playRoom,
    resetTick,
    isFirstTick,
    applyIOF
) where

import Graphics.Gloss(Picture)
import Graphics.Gloss.Game
import Engine.Base
import Game.Structure.GameState
import Game.Structure.MenuState
import Game.Rules.Rules
import Game.UI.Base
import Game.Context.SwitchRoom
import Game.Context.Persistant

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
    menuSwitch :: SwitchRoom,
    menuInputRules :: [MenuInputRule],
    firstTick :: Bool,
    ioF :: (Persistant -> IO Persistant)
}

type RoomUpdateFunc = (Float -> GameState -> GameState)

{- Instances -}
instance Inputable Room where
    input e r@Room{state, gameInputRules = gir} = r{state=nstate} where nstate = input e $ applyGameInputRules e gir state
    input e m@Menu{menuState=ms, menuInputRules = mir} = m{menuState=nstate{items = map (inputItem e) (items nstate)}}
        where nstate = applyMenuInputRules e mir ms

instance Renderable Room where
    render r@Room{state} = renderInstructions $ draw state
    render m@Menu{menuState} = renderInstructions $ concatMap drawItem $ items menuState

instance BaseUpdateable Room where
    baseUpdate dt r@Room{gameRules, rUpdate, state} = r{state=nstate} where nstate = applyGameRules gameRules $ rUpdate dt state
    baseUpdate dt m@Menu{menuState=ms} = m{menuState = nstate, menuSwitch = nswitch (items nstate)} 
        where
            nstate = ms{items = map (updateItem (selector ms) (menuOldPersistant ms) (menuNewPersistant ms)) (items ms)}
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

makeMenu :: [MenuItem] -> [MenuInputRule] -> Room
makeMenu items inputRules = Menu startState startState RoomStay inputRules True (\p -> do return p)
    where
        startState = makeMenuState items 0 

makeMenuF :: [MenuItem] -> [MenuInputRule] -> (Persistant -> IO Persistant) -> Room
makeMenuF items inputRules iof = Menu startState startState RoomStay inputRules True iof
    where
        startState = makeMenuState items 0 

resetTick r@Room{state} = r
resetTick m@Menu{firstTick} = m{firstTick = True}

isFirstTick r@Room{state} = False
isFirstTick m@Menu{firstTick = ft} = ft

applyIOF r@Room{state} = do return r
applyIOF m@Menu{menuState = ms, ioF = iof} = do
    newp <- iof $ menuOldPersistant ms
    return m{menuState = ms{menuOldPersistant = newp}}

playRoom f Room{ initState, rUpdate } =
    f initState render input [rUpdate]
