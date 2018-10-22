module Game.Agents.Pacman (
    pacman
) where

import Engine.Core.Coordinate
import Game.Agent
import Game.Agents.AgentTypes
import Resources(spritePacmanStill)
import Constants


{- Data structures -}


{- Classes -}


{- Instances -}


{- Functions -}
pacman :: Coordinate -> AgentBehaviour -> Agent
pacman position behaviour = Agent Pacman position DNone pacmanSpeed spritePacmanStill behaviour
