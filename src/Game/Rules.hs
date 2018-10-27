module Game.Rules (
    testRule, pacdotsRule
) where

import Game.SwitchRoom
import Game.GameState
import Game.Rule
import Game.Level.Level
import Game.Agents.AgentTypes
import Game.Agent
--TODO, uses internal??!
import Game.Internal

testRule :: Rule
testRule state@GameState{t = time}
    | time > 10 = state{switch = RoomSwitch "a" ResumeRoom}
    | otherwise = state
----------------------------------------------------------------
pacdotsRule :: Rule
pacdotsRule state@GameState{score = sc, world = wrld} = result $ applyRule allAgents (lvl, 0)
    where
        lvl = level wrld
        allAgents = agents wrld
        applyRule :: [Agent] -> (Level, Int) -> (Level, Int)
        applyRule [] l = l
        applyRule (a:as) l = applyRule as $ pacdotsOnPacman a l
        result (nlvl, inc) = state{score = sc + inc, world = wrld{level = nlvl}}

pacdotsOnPacman Agent{agentType = Pacman, position = pos} old@(lvl, inc)
    | onDot = (makeNewLvl, inc + 1)
    | otherwise = old
    where
        table = tiles lvl
        tablePos = coordinateToTile table pos
        tile = table ! tablePos
        onDot = tile == TilePowerup PacDot
        makeNewLvl = lvl{tiles = set table tablePos TileEmpty}
pacdotsOnPacman _ ding = ding
----------------------------------------------------------------
