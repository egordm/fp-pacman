-- | This module contains the data types
--   which represent the state of the game
module Model where

import Resources
import Engine.Graphics.Sprite


data InfoToShow = ShowNothing
                | ShowANumber Int
                | ShowAChar   Char

data GameState = GameState {
                   test  :: Sprite
                 , elapsedTime :: Float
                 }

initialState :: GameState
initialState = GameState spritePacmanRight 0