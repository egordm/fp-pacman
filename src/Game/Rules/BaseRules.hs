module Game.Rules.BaseRules (
    rulePacmanDotConsume,
    ruleGhostCatchPacman,
    rulePacmanDiedRestart,
    rulePacmanPowerpillConsume
) where

import Debug.Trace
import Engine.Base
import Game.Rules.Rule
import Game.Structure.Base
import Game.Level.Base
import Game.Agents.Base
import Game.Context.SwitchRoom
import Constants

{- Functions -}
-- | RULE -------------
-- | If pacman consumes a dot, dot disappears and score is incremented by one
rulePacmanDotConsume :: Rule
rulePacmanDotConsume s@GameState{world}
    = foldr agentConsumeDot s (filterAgentsPacman (agents world))

-- | Handles consuming the dot and inrementing the score
agentConsumeDot :: Agent -> GameState -> GameState
agentConsumeDot Agent{position} s@GameState{world=w@World{level=l}, scoreInfo=pscoreInfo}
    | tile == TilePowerup PacDot = s{scoreInfo = incrementScore pscoreInfo 1, world = nworld}
    | otherwise = s
      where pos = coordToTile (tiles l) position
            tile = tiles l ! pos
            nworld = w{level=l{tiles = set (tiles l) pos TileEmpty}}

-- | RULE -------------
-- | If pacman is caught by ghost, he dies
ruleGhostCatchPacman :: Rule
ruleGhostCatchPacman s@GameState{world=w@World{agents=pagents}}
    = s{world=w{agents=nagents}}
      where nagents = map (pacmanCheckCaught s) pagents

-- | Checks if current agent is pacman and sets died to true if pacman is in the same tile as a ghost
pacmanCheckCaught :: GameState -> Agent -> Agent
pacmanCheckCaught s@GameState{world=w@World{level, agents}} a@Agent{agentType=at@Pacman{died=False}}
    | isCaught = a{agentType=at{died=True}}
    | otherwise = a
      where ghosts = filter (\a -> not (isInScatterMode (agentType a))) (filterAgentsGhost agents)
            onSameTile o = coordDist (position a) (position o) < fromInteger tileSize
            isCaught = any (\o -> onSameTile o) ghosts
pacmanCheckCaught _ a = a


-- | RULE -------------
-- | If pacman has died and death animation has finished, game is reset
rulePacmanDiedRestart :: Rule
rulePacmanDiedRestart s@GameState{world=w@World{agents=pagents}, scoreInfo=pscoreInfo}
    | pacmanDied = reset s{scoreInfo = decrementLife pscoreInfo}
    | otherwise = s
      where pacmans = filterAgentsPacman pagents
            pacmanDied = any (\Agent{agentType, sprite} -> died agentType && animationEnded sprite) pacmans

-- | RULE -------------
-- | If pacman eats a powerpill, the scatter mode will start for an amount of ticks
rulePacmanPowerpillConsume :: Rule
rulePacmanPowerpillConsume s@GameState{world}
    = foldr agentConsumePowerpill s (filterAgentsPacman (agents world))

-- | Handles consuming the powerpill, turning on scatter mode and incrementing the score
agentConsumePowerpill :: Agent -> GameState -> GameState
agentConsumePowerpill Agent{position} s@GameState{world=w@World{level=l}, scoreInfo=pscoreInfo}
    | tile == TilePowerup PowerPill = s{scoreInfo = incrementScore pscoreInfo 10, world = nworld}
    | otherwise = s
      where pos = coordToTile (tiles l) position
            tile = tiles l ! pos
            nagents = map (agentSetScatterTicks scatterModeDuration) (agents w)
            nworld = w{level=l{tiles = set (tiles l) pos TileEmpty}, agents=nagents}
