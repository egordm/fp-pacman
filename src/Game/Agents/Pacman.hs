module Game.Agents.Pacman (
    pacman
) where

import Engine.Core.Base
import Game.Agents.Agent
import Game.Agents.AgentTypes
import Resources(spritePacmanStill)
import Constants


{- Data structures -}


{- Classes -}


{- Instances -}


{- Functions -}
-- | Creates a pacman with given behaviour
pacman :: Coordinate -> AgentBehaviour -> Agent
pacman position behaviour = Agent Pacman{died=False} position DNone pacmanSpeed spritePacmanStill behaviour posZ
