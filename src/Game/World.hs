{-# LANGUAGE NamedFieldPuns #-}

module Game.World (
    World(..),
    Updateable(..),
    Renderable(..)
) where

import Engine.Graphics.Sprite
import Engine.Graphics.Rendering
import Engine.Core.Classes
import Engine.Core.Coordinate
import Game.Internal(World(..))
import Game.Input
import Game.Agent

{- Data structures -}

{- Classes -}

{- Instances -}
instance Updateable World where
    update dt t w@World{agents} = w{agents=nagents}
                                  where nagents = map (updateAgent dt t w) agents

instance Renderable World where
    render World{agents} = renderInstructions agentDrawings
                                 where agentDrawings = map draw agents

instance Inputable World where
    input event w@World{agents} = w{agents = nagents}
                                  where nagents = map (input event) agents

{- Functions -}

