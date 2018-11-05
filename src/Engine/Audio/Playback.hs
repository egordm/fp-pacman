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
-- | Soundcall to select a sound from the sound dictionary
type SoundCall = (Sounds -> Sound)

-- | Action which can be called on the audio player
data PlayAction = PlayIfEnded | PlayForce | Stop deriving (Eq, Show)

-- | The repeat amount a sound should be repeated
type PlayRepeat = Mix.Times

-- | Sound instruction with details on how a sound should be played
data SoundInstruction = SoundInstruction PlayAction PlayRepeat SoundCall

{- Classes -}
-- | Class that produces sounds
class Soundable a where
    doSound :: a -> [SoundInstruction]

{- Instances -}
instance Show SoundInstruction where
    show (SoundInstruction action repeat _) = "SoundInstruction(" ++ show action ++ ")"

{- Functions -}
-- | Play list of given sound instructions
playSoundInstructions :: Sounds -> [SoundInstruction] -> IO ()
playSoundInstructions sounds instructions = mapM_ (playSoundInstructionRaw sounds) instructions

-- | Play soundinstruction given sound collection
playSoundInstructionRaw :: Sounds -> SoundInstruction -> IO ()
playSoundInstructionRaw sounds si@(SoundInstruction _ _ call) = playSoundInstruction si (call sounds)

-- | Play given sound
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

-- | PlayRepeat quick constructors
playForever, playOnce :: PlayRepeat
playForever = 0
playOnce = 1