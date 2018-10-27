module Game.Rules (
    testRule, pacmansOnPacdots, ghostsOnPacmansRule
) where

import Game.SwitchRoom
import Game.GameState
import Game.Rule
import Game.Level.Level
import Game.Agents.AgentTypes
import Game.Agent
import Game.World

testRule :: Rule
testRule state@GameState{t = time}
    | time > 10 = state{switch = RoomSwitch "a" ResumeRoom}
    | otherwise = state
----------------------------------------------------------------
pacmansOnPacdots :: Rule
pacmansOnPacdots state@GameState{score = sc, world = wrld} = result $ applyRule allAgents (lvl, 0)
    where
        lvl = level wrld
        allAgents = agents wrld
        applyRule :: [Agent] -> (Level, Int) -> (Level, Int)
        applyRule [] l = l
        applyRule (a:as) l = applyRule as $ _pacmanOnPacdot a l
        result (nlvl, inc) = state{score = sc + inc, world = wrld{level = nlvl}}

_pacmanOnPacdot Agent{agentType = Pacman, position = pos} old@(lvl, inc)
    | onDot = (makeNewLvl, inc + 1)
    | otherwise = old
    where
        table = tiles lvl
        tablePos = coordinateToTile table pos
        tile = table ! tablePos
        onDot = tile == TilePowerup PacDot
        makeNewLvl = lvl{tiles = set table tablePos TileEmpty}
_pacmanOnPacdot _ ding = ding
----------------------------------------------------------------
ghostsOnPacmansRule :: Rule
ghostsOnPacmansRule state@GameState{world = World{agents = agnts, level = lvl}}
    | didCol = state{switch = RoomReload}
    | otherwise = state
    where
        table = tiles lvl
        allCols = [_ghostOnPacman table g p | g <- agnts, p <- agnts]
        didCol = any (id) allCols

_ghostOnPacman table Agent{agentType = Ghost{}, position = pos0} Agent{agentType = Pacman, position = pos1} = 
    tablePos0 == tablePos1
    where
        tablePos0 = coordinateToTile table pos0
        tablePos1 = coordinateToTile table pos1
_ghostOnPacman _ _ _ = False