module Game.Graphics.Text (

) where
    
import Engine.Base
import Constants
import Resources

    
{- Data structures -}
data FontString = FontString [Char] Coordinate


{- Classes -}


{- Instances -}


{- Functions -}
charToSprite :: Char -> Sprite
charToSprite c = case charIdx of
                  (Just i) -> i ! spriteFont
                  Nothing -> createEmptySprite
                 where charIdx = elemIndex c fontMapping
