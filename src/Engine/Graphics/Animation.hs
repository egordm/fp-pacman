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

{- Classes -}

{- Instances -}
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
emptyAnimationState :: AnimationState
emptyAnimationState = AnimationState 0 0

