module Game.Structure.GameState (
    GameState(..),
    BackgroundSound(..),
    Updateable(..),
    Renderable(..),
    Inputable(..),
    gamestate,
    addSound,
    addEffect,
    stopAllSounds
) where

import Debug.Trace
import Engine.Core.Base
import Engine.Audio.Base
import Game.Structure.World
import Game.Structure.ScoreHolder
import Game.Input.Base
import Game.Agents.Base
import Game.Context.SwitchRoom
import Game.Context.Persistant
import Resources

{- Data structures -}
data GameState = GameState {
                     t :: Float,
                     world :: World,
                     scoreInfo :: ScoreHolder,
                     switch :: SwitchRoom,
                     soundInstructions :: [SoundInstruction],
                     bgSound :: BackgroundSound,
                     gameOldPersistant :: Persistant,
                     gameNewPersistant :: Persistant
                 } deriving (Show)

data BackgroundSound = None | Intro | ScatterMode | SirenSlow | SirenMedium | SirenFast | DeathSound deriving (Show, Eq)

{- Classes -}


{- Instances -}
instance Updateable GameState where
    update dt nt s@GameState{t=pt, world=pworld} = s{t=nt, world=update dt nt pworld, soundInstructions=[]}

instance Drawable GameState where
    draw GameState{world, scoreInfo} = draw world ++ draw scoreInfo

instance Inputable GameState where
    input event s@GameState{world=pworld} = s{world=input event pworld}

instance Resetable GameState where
    reset s@GameState{world=pworld} = stopAllSounds s{world=reset pworld}

instance Soundable GameState where
    doSound GameState{soundInstructions} = soundInstructions

{- Functions -}
gamestate level bois = stopAllSounds (GameState 0 world scoreholder RoomStay [] None emptyPersistant emptyPersistant)
                       where baseWorld = World level []
                             world = addAgents bois baseWorld

addSound :: PlayAction -> PlayRepeat -> SoundCall -> GameState -> GameState
addSound action repeat soundCall state
    = state{soundInstructions=SoundInstruction action repeat soundCall : soundInstructions state}

addEffect :: SoundCall -> GameState -> GameState
addEffect soundCall state
    = state{soundInstructions=SoundInstruction PlayForce playOnce soundCall : soundInstructions state}

stopAllSounds :: GameState -> GameState
stopAllSounds s = foldr (addSound Stop playOnce) s calls
                  where calls = [soundFruit, soundGhostEat1, soundExtraMan, soundDeath3, soundMunchA]