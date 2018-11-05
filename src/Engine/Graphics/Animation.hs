module Engine.Graphics.Animation (
     Animation(..),
     AnimationType(..),
     AnimationState(..),
     Updateable(..),
     emptyAnimationState
) where
    
import Constants
import Engine.Core.Classes

{- Data structures -}
-- | Animation type
data AnimationType = Single
                   | Repeating
                   deriving (Show, Eq)

-- | Animation state which changes everytime
data AnimationState = AnimationState {
                          current :: Int,         -- Current sprite in loop
                          changeTime :: Float     -- Time till sprite change
                      } deriving (Show, Eq)

-- | Animation definition
data Animation = Animation {
                     animType :: AnimationType,
                     animState :: AnimationState,
                     frameCount :: Int,    -- N of frames in loop
                     interval :: Float     -- Duration between sprite changes
                 } deriving (Show)

{- Classes -}

{- Instances -}
instance Eq Animation where
    (==) Animation{animType=animType1} Animation{animType=animType2} = animType1 == animType2

-- | Update the animation state. Repeatable should loop etc
instance Updateable Animation where
    update dt t a@(Animation animType (AnimationState current changeTime) frameCount interval)
        = a{animState = AnimationState nCurrent nChangeTime}
          where nextFrameFn = case animType of Single    -> \s -> min (s + 1) (frameCount - 1)
                                               Repeating -> \s -> (s + 1) `mod` frameCount
                nCurrent | changeTime <= 0 = nextFrameFn current
                         | otherwise       = current
                nChangeTime | changeTime <= 0 = changeTime + interval - dt
                            | otherwise       = changeTime - dt

{- Functions -}
-- | Create empty animation state
emptyAnimationState :: AnimationState
emptyAnimationState = AnimationState 0 0

