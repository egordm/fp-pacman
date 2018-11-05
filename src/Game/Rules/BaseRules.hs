module Game.Rules.BaseRules (
    rulePacmanDotConsume,
    rulePacmanPowerpillConsume,
    ruleGhostCatchPacman,
    rulePacmanDiedRestart,
    rulePacmanEatGhost,
    ruleGhostRevives,
    ruleGhostRelease,
    ruleBackgroundSound
) where

import Debug.Trace
import Engine.Base
import Game.Rules.Rules
import Game.Rules.Helpers
import Game.Structure.Base
import Game.Level.Base
import Game.Agents.Base
import Game.Context.SwitchRoom
import Constants
import Resources
import Game.Context.Persistant

{- Functions -}
-- | RULE -------------
-- | If pacman consumes a dot, dot disappears and score is incremented by one
rulePacmanDotConsume :: GameRule
rulePacmanDotConsume s@GameState{world}
    = predicateFoldl condition pacmanDotConsume s pacmans
      where pacmans = filterAgentsPacman (agents world)
            condition a GameState{world} = agentOnTile a (level world) (TilePowerup PacDot)

-- | Handles consumption of a dot
pacmanDotConsume :: Agent -> GameState -> GameState
pacmanDotConsume a s@GameState{world=w@World{level}, scoreInfo}
    = addEffect soundMunchA s{world=w{level=nlevel}, scoreInfo=nscore}
      where nlevel = setl level (agentPos a level) TileEmpty
            nscore = incrementScore scoreInfo scorePacdot

-- | RULE -------------
-- | If pacman eats a powerpill, the scatter mode will start for an amount of ticks
rulePacmanPowerpillConsume :: GameRule
rulePacmanPowerpillConsume gs@GameState{world}
    = predicateFoldl condition pacmanPowerpillConsume gs pacmans
      where pacmans = filterAgentsPacman (agents world)
            condition a GameState{world} = agentOnTile a (level world) (TilePowerup PowerPill)

-- | Handles consuming the powerpill, turning on scatter mode and incrementing the score
pacmanPowerpillConsume :: Agent -> GameState -> GameState
pacmanPowerpillConsume a s@GameState{world=w@World{level, agents}, scoreInfo}
    = addEffect soundMunchB s{world=w{level=nlevel, agents=nagents}, scoreInfo=nscore}
      where nlevel = setl level (agentPos a level) TileEmpty
            nscore = incrementScore scoreInfo scorePacdot
            nagents = map (agentSetScatterTicks scatterModeDuration) (agents)

-- | RULE -------------
-- | If pacman is caught by ghost, he dies
ruleGhostCatchPacman :: GameRule
ruleGhostCatchPacman s@GameState{world=w@World{agents}, scoreInfo}
    = nstate{world=w{agents=nagents}}
      where nagents = predicateMap condition (agentSetDied True) agents
            ghosts = filter (\a -> not (isInScatterMode (agentType a))) (filterAgentsGhost agents)
            condition = compoundPredicate [(== Pacman{}) . agentType, not . died . agentType, anyAgentSameTile ghosts]
            nstate = predicateFoldl (\a gs -> condition a) addSoundEffect s agents
            addSoundEffect a gs = setBackgroundSound DeathSound playOnce (pacmanDiedSound (lives scoreInfo)) gs

pacmanDiedSound :: Int -> SoundCall
pacmanDiedSound livesLeft = case livesLeft of
    1 -> soundDeath2
    _ -> soundDeath1

-- | RULE -------------
-- | If pacman has died and death animation has finished, game is reset
rulePacmanDiedRestart :: GameRule
rulePacmanDiedRestart s@GameState{world=w@World{agents=pagents}, scoreInfo=pscoreInfo, gameNewPersistant = ognp}
    | pacmanDied && (isGameOver == False) = reset s{scoreInfo = decrementLife pscoreInfo}
    | pacmanDied && (isGameOver == True) = s{switch = RoomSwitch "gameover" ReloadRoom, gameNewPersistant = gnp}
    | otherwise = s
      where pacmans = filterAgentsPacman pagents
            pacmanDied = any (\Agent{agentType, sprite} -> died agentType && animationEnded sprite) pacmans
            isGameOver = (lives pscoreInfo) <= 1 --because this is before we decrement lives
            gnp = addInt ognp "score" $ score pscoreInfo

-- | RULE -------------
-- | If ghost is eaten by pacman in scatter mode, ghost dies
rulePacmanEatGhost :: GameRule
rulePacmanEatGhost s@GameState{world=w@World{agents}}
    = nstate{world=w{agents=nagents}}
      where nagents = predicateMap condition (agentSetDied True) agents
            pacmans = filterAgentsPacman agents
            condition = compoundPredicate [isGhost . agentType, not . died . agentType, isInScatterMode . agentType, anyAgentSameTile pacmans]
            nstate = predicateFoldl (\a gs -> condition a) addSoundEffect s agents
            addSoundEffect a gs = addEffect soundGhostEat2 gs

-- | RULE -------------
-- | If ghost is dead and arrives back in the cage. Then it comes back to live
ruleGhostRevives :: GameRule
ruleGhostRevives s@GameState{world=w@World{agents, level}}
    = s{world=w{agents=nagents}}
      where nagents = predicateMap condition action agents
            marker = markerCoordinate markerRevivalPoint level
            action a = a{agentType=(agentType a){died=False, scatterTicks=0, caged=True}}
            condition = compoundPredicate [isGhost . agentType, died . agentType, \x -> onAgentOverlapsPos (fromInteger tileSize/4) x marker]

-- | RULE -------------
-- | If ghost is caged and pacman has eaten enough dots, the ghost will be released
ruleGhostRelease :: GameRule
ruleGhostRelease s@GameState{world=w@World{agents, level}, scoreInfo=ScoreHolder{score}}
    = s{world=w{agents=nagents}}
      where nagents = predicateMap condition action agents
            action a = a{agentType=(agentType a){caged=False}}
            condition = compoundPredicate [isGhost . agentType, caged . agentType, ((>=) score) . dotsUntilRelease . agentType]


-- | RULE SORTOF -------------
-- | Plays the appropriate background audio
ruleBackgroundSound :: GameRule
ruleBackgroundSound s@GameState{t, world=w@World{agents, level}, scoreInfo}
    | t < 3 = setBackgroundSound Intro playOnce soundIntro s
    | isScatterMode = setBackgroundSound ScatterMode playForever soundLargePelletLoop s
    | not isPacmanDead && score scoreInfo > 1500 = setBackgroundSound SirenSlow playForever soundSirenFast s
    | not isPacmanDead && score scoreInfo > 600 = setBackgroundSound SirenSlow playForever soundSirenMedium s
    | not isPacmanDead = setBackgroundSound SirenSlow playForever soundSirenSlow s
    | otherwise = s
      where isPacmanDead = any (died . agentType) (filterAgentsPacman agents)
            isScatterMode = any (((<) 0) . scatterTicks . agentType) (filterAgentsGhost agents)

-- | Sets background sound and tries to take into account already playing ones by waiting or changing sound
setBackgroundSound :: BackgroundSound -> PlayRepeat -> SoundCall -> GameState -> GameState
setBackgroundSound soundType repeat soundCall s@GameState{bgSound}
    | bgSound == soundType = startBgNonIntrusive s
    | bgSound == Intro || bgSound == DeathSound = startBgNonIntrusive s{bgSound=soundType}
    | otherwise = stopBackgroundSound $ startBgNonIntrusive s{bgSound=soundType}
      where startBgNonIntrusive = addSound PlayIfEnded repeat soundCall