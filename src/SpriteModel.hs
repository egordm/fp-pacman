module SpriteModel (
    Sprite,
    createAnimatedSprite,
    createStaticSprite,
    Animation,
    AnimationType,
    AnimationState
) where

import Graphics.Gloss (Picture)

data AnimationType = Single | Repeating deriving (Show)

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

emptyAnimationState :: AnimationState
emptyAnimationState = AnimationState 0 0

createAnimatedSprite :: AnimationType -> [Picture] -> Float -> Sprite
createAnimatedSprite animType frames interval = AnimatedSprite anim frames
                                                where anim = Animation animType emptyAnimationState (length frames) interval

createStaticSprite :: Picture -> Sprite
createStaticSprite frame = StaticSprite frame