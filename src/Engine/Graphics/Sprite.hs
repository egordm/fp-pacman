module Engine.Graphics.Sprite (
    Sprite(..),
    Renderable(..),
    Updateable(..),
    createAnimatedSprite,
    createStaticSprite,
    createEmptySprite
) where

import Graphics.Gloss (Picture(..))
import Engine.Core.Classes
import Engine.Graphics.Animation

{- Data structures -}
data Sprite = StaticSprite Picture
            | AnimatedSprite {
                animation :: Animation,
                frames :: [Picture]
            }
            deriving (Show, Eq)

{- Classes -}
class Renderable a where
    render :: a -> Picture

{- Instances -}
instance Renderable Sprite where
    render (StaticSprite frame) = frame
    render (AnimatedSprite Animation{animState=AnimationState current _} frames) = frames!!current

instance Updateable Sprite where
    update dt t s@(StaticSprite _) = s
    update dt t s@(AnimatedSprite a _) = s{animation = update dt t a}

{- Functions -}
createAnimatedSprite :: AnimationType -> [Picture] -> Float -> Sprite
createAnimatedSprite animType frames interval = AnimatedSprite anim frames
                                                where anim = Animation animType emptyAnimationState (length frames) interval

createStaticSprite :: Picture -> Sprite
createStaticSprite = StaticSprite

createEmptySprite :: Sprite
createEmptySprite = StaticSprite Blank
