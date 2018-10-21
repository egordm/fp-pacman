module Engine.Core.Classes (
    Updateable(..),
) where


class Updateable a where
    update :: Float -> Float -> a -> a

