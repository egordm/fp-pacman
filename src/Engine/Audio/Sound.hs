module Engine.Audio.Sound (
  Sound(..)
) where
    
import qualified SDL
import qualified SDL.Mixer as Mix

    
{- Data structures -}
data Sound = Sound {
                signal :: Mix.Chunk,
                channel :: Integer,
                volume :: Integer
             }



{- Classes -}


{- Instances -}


{- Functions -}
