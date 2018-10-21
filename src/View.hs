-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

import Resources
import Engine.Graphics.Models
import Engine.Graphics.Rendering
import Engine.Coordinates

render :: GameState -> Picture
render _ = draw (DrawInstruction halfScreenSize spritePacmanDie)
