-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

import Resources
import Engine.Graphics.Rendering
import Engine.Core.Coordinate

render :: GameState -> Picture
render (GameState sprite t) = drawInstructions [DrawInstruction halfScreenSize sprite]
