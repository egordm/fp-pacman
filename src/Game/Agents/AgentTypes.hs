module Game.Agents.AgentTypes (
    AgentType(..),
    GhostType(..),
) where

import Engine.Core.Coordinate

{- Data structures -}
data GhostType = Blinky | Pinky | Inky | Clyde deriving (Eq, Ord, Show)

data AgentType = Pacman | Ghost GhostType deriving (Eq, Show)

{- Classes -}


{- Instances -}


{- Functions -}

