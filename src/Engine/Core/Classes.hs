module Engine.Core.Classes (
    Updateable(..),
    BaseUpdateable(..),
    Resetable(..)
) where


class Updateable a where
    update :: Float -> Float -> a -> a

class BaseUpdateable a where
    baseUpdate :: Float -> a -> a

class Resetable a where
    reset :: a -> a
