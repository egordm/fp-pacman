module Game.Rule (
    Rule
) where

import Game.GameState

type Rule = (GameState -> GameState)