module Engine.Audio.Loading (
  loadSound
) where
    
import qualified SDL
import qualified SDL.Mixer as Mix
import Engine.Audio.Sound
import Constants

    
{- Data structures -}


{- Classes -}


{- Instances -}


{- Functions -}
loadSound :: String -> IO Sound
loadSound identifier = (Mix.load (resourceDir ++ identifier ++ ".wav"))