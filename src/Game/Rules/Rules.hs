module Game.Rules.Rules (
    GameRule,
    applyRules
) where

import Control.Arrow
import Game.Structure.GameState

{- Data structures -}
type GameRule = (GameState -> GameState)

{- Classes -}


{- Instances -}


{- Functions -}
applyRules :: [GameRule] -> GameState -> GameState
applyRules rls st = (foldl (>>>) id rls) st
