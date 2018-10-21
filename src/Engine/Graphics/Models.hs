module Engine.Graphics.Models (
    Sprite(..),
    createAnimatedSprite,
    createStaticSprite,
    createEmptySprite,
    Animation(..),
    AnimationType(..),
    AnimationState(..),
    DrawInstruction(..)
) where

import Constants
import Graphics.Gloss (Picture(Blank), translate)
import Engine.Coordinates

-- Data structures
data AnimationType = Single
                   | Repeating
                   deriving (Show)

data AnimationState = AnimationState {
                          current :: Int,         -- Current sprite in loop
                          changeTime :: Float     -- Time till sprite change
                      } deriving (Show)

data Animation = Animation {
                     animType :: AnimationType,
                     animState :: AnimationState,
                     frameCount :: Int,    -- N of frames in loop
                     interval :: Float     -- Duration between sprite changes
                 } deriving (Show)

data Sprite = StaticSprite Picture
            | AnimatedSprite Animation [Picture]
            deriving (Show)

data DrawInstruction = DrawInstruction Coordinate Sprite

-- Creation functions
emptyAnimationState :: AnimationState
emptyAnimationState = AnimationState 0 0

createAnimatedSprite :: AnimationType -> [Picture] -> Float -> Sprite
createAnimatedSprite animType frames interval = AnimatedSprite anim frames
                                                where anim = Animation animType emptyAnimationState (length frames) interval

createStaticSprite :: Picture -> Sprite
createStaticSprite frame = StaticSprite frame

createEmptySprite :: Sprite
createEmptySprite = StaticSprite Blank

