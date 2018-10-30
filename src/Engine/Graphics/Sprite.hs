module Engine.Graphics.Sprite (
    Sprite(..),
    Renderable(..),
    Updateable(..),
    createAnimatedSprite,
    createStaticSprite,
    createEmptySprite,
    animationEnded
) where

import Graphics.Gloss (Picture(..))
import Engine.Core.Classes
import Engine.Graphics.Animation

{- Data structures -}
data Sprite = StaticSprite {
                frame :: Picture,
                source :: String
              }
            | AnimatedSprite {
                animation :: Animation,
                frames :: [Sprite]
              }
            deriving (Show, Eq)

{- Classes -}
class Renderable a where
    render :: a -> Picture

{- Instances -}
instance Renderable Sprite where
    render (StaticSprite{frame}) = frame
    render (AnimatedSprite Animation{animState=AnimationState current _} frames) = frame (frames!!current)

instance Updateable Sprite where
    update dt t s@(StaticSprite{}) = s
    update dt t s@(AnimatedSprite a _) = s{animation = update dt t a}

{- Functions -}
createAnimatedSprite :: AnimationType -> [Sprite] -> Float -> Sprite
createAnimatedSprite animType frames interval = AnimatedSprite anim frames
                                                where anim = Animation animType emptyAnimationState (length frames) interval

createStaticSprite :: Picture -> String -> Sprite
createStaticSprite = StaticSprite

createEmptySprite :: Sprite
createEmptySprite = StaticSprite Blank "blank"

animationEnded :: Sprite -> Bool
animationEnded StaticSprite{} = True
animationEnded AnimatedSprite{animation=Animation{animType=Repeating}} = False
animationEnded AnimatedSprite{animation=Animation{animType=Single, animState=AnimationState{current}, frameCount}}
    | current == frameCount - 1 = True
    | otherwise = False
