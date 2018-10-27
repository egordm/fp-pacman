module Game.Rules.Rule (
    Rule,
    applyRules
) where

import Control.Arrow
import Game.Structure.GameState

{- Data structures -}
type Rule = (GameState -> GameState)


{- Classes -}


{- Instances -}


{- Functions -}
applyRules :: [Rule] -> GameState -> GameState
applyRules rls st = (foldl (>>>) id rls) st
