module Game.Rules.Helpers where
    
import Engine.Base
import Game.Agents.Base
import Game.Level.Base
import Constants

{- Data structures -}


{- Classes -}


{- Instances -}


{- Functions -}
opAgentOverlapsPos :: Float -> Agent -> Coordinate -> Bool
opAgentOverlapsPos maxDist a bc = coordDist (position a) bc < maxDist

onAgentsOverlap :: Float -> Agent -> Agent -> Bool
onAgentsOverlap maxDist a b = coordDist (position a) (position b) < maxDist

onAgentsSameTile :: Agent -> Agent -> Bool
onAgentsSameTile = onAgentsOverlap (fromInteger tileSize)

anyAgentSameTile :: [Agent] -> Agent -> Bool
anyAgentSameTile as a = any (==True) (map (onAgentsSameTile a) as)

agentSetDied :: Bool -> Agent -> Agent
agentSetDied v a = a{agentType=(agentType a){died=v}}

agentPos :: Agent -> Level -> Pos
agentPos Agent{position} Level{tiles} = coordToTile tiles position

agentOnTile :: Agent -> Level -> Tile -> Bool
agentOnTile a l t = tiles l ! agentPos a l == t



predicateMap :: (a -> Bool) -> (a -> a) -> [a] -> [a]
predicateMap _ _ [] = []
predicateMap p f (x:xs) | p x = f x : predicateMap p f xs
                        | otherwise = x : predicateMap p f xs

compoundPredicate :: [(a -> Bool)] -> a -> Bool
compoundPredicate ps a = all (==True) (map (\p -> p a) ps)

predicateFoldr :: (a -> b -> Bool) -> (a -> b -> b) -> b -> [a] -> b
predicateFoldr _ _ r [] = r
predicateFoldr p f r (x:xs) | p x r = predicateFoldr p f (f x r) xs
                            | otherwise = predicateFoldr p f r xs
