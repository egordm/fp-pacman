module Engine.Core.Classes (
    Updateable(..),
    BaseUpdateable(..),
    Resetable(..)
) where

-- | Updateable object with time given by context
class Updateable a where
    update :: Float -> Float -> a -> a

-- | Updateable object which does not depend on a context
class BaseUpdateable a where
    baseUpdate :: Float -> a -> a

-- | Resetable object
class Resetable a where
    reset :: a -> a
