module Game.UI.Text (
    FontString(..),
    putInDraws
) where

import Engine.Base
import Constants
import Resources
import Data.List

{- Data structures -}
data FontString = FontString [Char] Coordinate


{- Classes -}


{- Instances -}

instance Drawable FontString where
    draw (FontString string coor) = putInDraws string coor

{- Functions -}
putInDraws :: [Char] -> Coordinate -> [DrawInstruction]
putInDraws [] _ = []
putInDraws (c:cs) (Coordinate x y) =
    (DrawInstruction (Coordinate x y) (charToSprite c)) : putInDraws cs (Coordinate (x + (8*spriteScale)) y)

charToSprite :: Char -> Sprite
charToSprite c = case charIdx of
    (Just i) -> spriteFont !! i
    Nothing -> createEmptySprite
    where charIdx = elemIndex c fontMapping
