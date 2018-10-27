module Engine.Core.Classes (
    Updateable(..),
    BaseUpdateable(..)
) where


class Updateable a where
    update :: Float -> Float -> a -> a

class BaseUpdateable a where
    baseUpdate :: Float -> a -> a