-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Engine.Graphics.Rendering

-- | Handle one iteration of the game
updateGame :: Float -> GameState -> GameState
updateGame dt gstate = gstate { test = update dt t (test gstate), elapsedTime = t}
                     where t = elapsedTime gstate + dt

-- | Handle user input
input :: Event -> GameState -> GameState
input e state = state
