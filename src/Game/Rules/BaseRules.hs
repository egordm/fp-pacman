module Game.Rules.BaseRules (
    rulePacmanDotConsume,
    ruleGhostCatchPacman,
    rulePacmanDiedRestart
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
    = foldr agentConsumeDot s pacmans
      where pacmans = filterAgentsPacman (agents world)

-- | Handles consuming the dot and inrementing the score per agent basis
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
      where ghosts = filterAgentsGhost agents
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