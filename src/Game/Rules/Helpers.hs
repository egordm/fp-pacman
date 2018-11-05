module Game.Rules.Helpers where
    
import Engine.Base
import Game.Agents.Base
import Game.Level.Base
import Game.Structure.Base
import Constants
import Resources

{- Data structures -}


{- Classes -}


{- Instances -}


{- Functions -}
-- | Check if agent and a coordinate overlap
onAgentOverlapsPos :: Float -> Agent -> Coordinate -> Bool
onAgentOverlapsPos maxDist a bc = coordDist (position a) bc < maxDist

-- | Check if agents positions overlap
onAgentsOverlap :: Float -> Agent -> Agent -> Bool
onAgentsOverlap maxDist a b = coordDist (position a) (position b) < maxDist

-- | Check if agents are on the same tiles
onAgentsSameTile :: Agent -> Agent -> Bool
onAgentsSameTile = onAgentsOverlap (fromInteger tileSize)

-- | Check if any agent in given list is on the same tile
anyAgentSameTile :: [Agent] -> Agent -> Bool
anyAgentSameTile as a = any (==True) (map (onAgentsSameTile a) as)

-- | Set agent dead
agentSetDied :: Bool -> Agent -> Agent
agentSetDied v a = a{agentType=(agentType a){died=v}}

-- | Get agent position
agentPos :: Agent -> Level -> Pos
agentPos Agent{position} Level{tiles} = coordToTile tiles position

-- | Check if agent is on the given tile
agentOnTile :: Agent -> Level -> Tile -> Bool
agentOnTile a l t = tiles l ! agentPos a l == t

-- | Stops the background audio by adding a sound instruction for that
stopBackgroundSound :: GameState -> GameState
stopBackgroundSound s = addSound Stop playForever soundSirenSlow s

-- ------ My cool highorder functions. They make the rule functions a LOT shorter
-- | Check if an item fulfills a predicate if it does, apply a function to it. Can be extended to 2 function for true and false
predicateMap :: (a -> Bool) -> (a -> a) -> [a] -> [a]
predicateMap _ _ [] = []
predicateMap p f (x:xs) | p x = f x : predicateMap p f xs
                        | otherwise = x : predicateMap p f xs

-- | Combine given predicates
compoundPredicate :: [(a -> Bool)] -> a -> Bool
compoundPredicate ps a = all (==True) (map (\p -> p a) ps)

-- | Basically foldl but checks if predicate is fulfilled. If it is, then a function is applied
predicateFoldl :: (a -> b -> Bool) -> (a -> b -> b) -> b -> [a] -> b
predicateFoldl _ _ r [] = r
predicateFoldl p f r (x:xs) | p x r = predicateFoldl p f (f x r) xs
                            | otherwise = predicateFoldl p f r xs
