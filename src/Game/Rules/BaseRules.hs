module Game.Rules.BaseRules (
    rulePacmanDotConsume,
    rulePacmanPowerpillConsume,
    ruleGhostCatchPacman,
    rulePacmanDiedRestart,
    rulePacmanEatGhost,
    ruleGhostRevives,
    ruleGhostRelease
) where

import Debug.Trace
import Engine.Base
import Game.Rules.Rule
import Game.Rules.Helpers
import Game.Structure.Base
import Game.Level.Base
import Game.Agents.Base
import Game.Context.SwitchRoom
import Constants

{- Functions -}
-- | RULE -------------
-- | If pacman consumes a dot, dot disappears and score is incremented by one
rulePacmanDotConsume :: Rule
rulePacmanDotConsume gs@GameState{world}
    = predicateFoldr condition pacmanDotConsume gs pacmans
      where pacmans = filterAgentsPacman (agents world)
            condition a GameState{world} = agentOnTile a (level world) (TilePowerup PacDot)

-- | Handles consumption of a dot
pacmanDotConsume :: Agent -> GameState -> GameState
pacmanDotConsume a s@GameState{world=w@World{level}, scoreInfo}
    = s{world=w{level=nlevel}, scoreInfo=nscore}
      where nlevel = setl level (agentPos a level) TileEmpty
            nscore = incrementScore scoreInfo scorePacdot

-- | RULE -------------
-- | If pacman eats a powerpill, the scatter mode will start for an amount of ticks
rulePacmanPowerpillConsume :: Rule
rulePacmanPowerpillConsume gs@GameState{world}
    = predicateFoldr condition pacmanPowerpillConsume gs pacmans
      where pacmans = filterAgentsPacman (agents world)
            condition a GameState{world} = agentOnTile a (level world) (TilePowerup PowerPill)

-- | Handles consuming the powerpill, turning on scatter mode and incrementing the score
pacmanPowerpillConsume :: Agent -> GameState -> GameState
pacmanPowerpillConsume a s@GameState{world=w@World{level, agents}, scoreInfo}
    = s{world=w{level=nlevel, agents=nagents}, scoreInfo=nscore}
      where nlevel = setl level (agentPos a level) TileEmpty
            nscore = incrementScore scoreInfo scorePacdot
            nagents = map (agentSetScatterTicks scatterModeDuration) (agents)

-- | RULE -------------
-- | If pacman is caught by ghost, he dies
ruleGhostCatchPacman :: Rule
ruleGhostCatchPacman s@GameState{world=w@World{agents}}
    = s{world=w{agents=nagents}}
      where nagents = predicateMap condition (agentSetDied True) agents
            ghosts = filter (\a -> not (isInScatterMode (agentType a))) (filterAgentsGhost agents)
            condition = compoundPredicate [(== Pacman{}) . agentType, not . died . agentType, anyAgentSameTile ghosts]

-- | RULE -------------
-- | If pacman has died and death animation has finished, game is reset
rulePacmanDiedRestart :: Rule
rulePacmanDiedRestart s@GameState{world=w@World{agents=pagents}, scoreInfo=pscoreInfo}
    | pacmanDied = reset s{scoreInfo = decrementLife pscoreInfo} -- IF lives < 0 just gameover
    | otherwise = s
      where pacmans = filterAgentsPacman pagents
            pacmanDied = any (\Agent{agentType, sprite} -> died agentType && animationEnded sprite) pacmans

-- | RULE -------------
-- | If ghost is eaten by pacman in scatter mode, ghost dies
rulePacmanEatGhost :: Rule
rulePacmanEatGhost s@GameState{world=w@World{agents}}
    = s{world=w{agents=nagents}}
      where nagents = predicateMap condition (agentSetDied True) agents
            pacmans = filterAgentsPacman agents
            condition = compoundPredicate [isGhost . agentType, not . died . agentType, isInScatterMode . agentType, anyAgentSameTile pacmans]

-- | RULE -------------
-- | If ghost is dead and arrives back in the cage. Then it comes back to live
ruleGhostRevives :: Rule
ruleGhostRevives s@GameState{world=w@World{agents, level}}
    = s{world=w{agents=nagents}}
      where nagents = predicateMap condition action agents
            marker = markerCoordinate markerRevivalPoint level
            action a = a{agentType=(agentType a){died=False, scatterTicks=0, caged=True}}
            condition = compoundPredicate [isGhost . agentType, died . agentType, \x -> opAgentOverlapsPos (fromInteger tileSize/4) x marker]

-- | RULE -------------
-- | If ghost is caged and pacman has eaten enough dots, the ghost will be released
ruleGhostRelease :: Rule
ruleGhostRelease s@GameState{world=w@World{agents, level}, scoreInfo=ScoreHolder{score}}
    = s{world=w{agents=nagents}}
      where nagents = predicateMap condition action agents
            action a = a{agentType=(agentType a){caged=False}}
            condition = compoundPredicate [isGhost . agentType, caged . agentType, ((>=) score) . dotsUntilRelease . agentType]
