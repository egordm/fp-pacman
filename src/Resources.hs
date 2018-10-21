module Resources (
    spritePacmanDie,
    spritePacmanRight
) where

import Engine.Graphics.Animation
import Engine.Graphics.Loading
import Constants


spritePacmanDie = loadAnimatedSprite "pacman/die" 11 Single spriteInterval
spritePacmanRight = loadAnimatedSprite "pacman/right" 2 Repeating spriteInterval