module Game.Rules.Rule (
    Rule
) where

import Game.Structure.GameState

type Rule = (GameState -> GameState)