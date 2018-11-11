module Game.Context.Persistant (
    Persistant(..), PersistantStrings, PersistantInts,
    emptyPersistant, addString, addInt, getString, getInt
) where

import qualified Data.Map as Map

{- Data structures -}

type PersistantStrings = (Map.Map String String)
type PersistantInts = (Map.Map String Int)
--datatype to transfer data from room to room
data Persistant = Persistant {
    strings :: PersistantStrings,
    ints :: PersistantInts
} deriving (Show)

{- Instances -}

{- Functions -}

emptyPersistant = Persistant Map.empty Map.empty

addString :: Persistant -> String -> String -> Persistant
addString pdata name value = pdata{strings = Map.insert name value $ strings pdata}

addInt :: Persistant -> String -> Int -> Persistant
addInt pdata name value = pdata{ints = Map.insert name value $ ints pdata}

getString :: Persistant -> String -> Maybe String
getString pdata name = Map.lookup name $ strings pdata

getInt :: Persistant -> String -> Maybe Int
getInt pdata name = Map.lookup name $ ints pdata
