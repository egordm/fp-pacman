module Engine.Audio.Playback (
  SoundCall(..),
  PlayAction(..),
  SoundInstruction(..),
  Soundable(..),
  PlayRepeat(..),
  playForever,
  playOnce,
  playSoundInstructions
) where

import Control.Monad
import qualified SDL
import qualified SDL.Mixer as Mix
import Engine.Audio.Sound
import Resources

{- Data structures -}
type SoundCall = (Sounds -> Sound)

data PlayAction = PlayIfEnded | PlayForce | Stop deriving (Eq, Show)

type PlayRepeat = Mix.Times

data SoundInstruction = SoundInstruction PlayAction PlayRepeat SoundCall

{- Classes -}
class Soundable a where
    doSound :: a -> [SoundInstruction]

{- Instances -}
instance Show SoundInstruction where
    show (SoundInstruction action repeat _) = "SoundInstruction(" ++ show action ++ ")"

{- Functions -}
playSoundInstructions :: Sounds -> [SoundInstruction] -> IO ()
playSoundInstructions sounds instructions = mapM_ (playSoundInstructionRaw sounds) instructions

playSoundInstructionRaw :: Sounds -> SoundInstruction -> IO ()
playSoundInstructionRaw sounds si@(SoundInstruction _ _ call) = playSoundInstruction si (call sounds)

playSoundInstruction :: SoundInstruction -> Sound -> IO ()
playSoundInstruction (SoundInstruction PlayForce repeat _) Sound{signal, channel, volume} = do
    Mix.playOn (fromInteger channel) repeat signal
    return ()

playSoundInstruction (SoundInstruction PlayIfEnded repeat _) Sound{signal, channel, volume} = do
    isPlaying <- Mix.playing (fromInteger channel)
    when (not isPlaying) $ pure () <* Mix.playOn (fromInteger channel) repeat signal
    return ()

playSoundInstruction (SoundInstruction Stop _ _) Sound{signal, channel, volume} = do
    Mix.halt (fromInteger channel)
    return ()

playForever, playOnce :: PlayRepeat
playForever = 0
playOnce = 1