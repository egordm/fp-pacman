module Game.Menu.MenuShared (
    basicSelectorRule, highScoreIO, updateHSLabel
) where

import Engine.Base
import Graphics.Gloss.Game
import Game.UI.Base
import Game.Context.SwitchRoom
import Game.Context.Room
import Game.Structure.Base
import Game.Context.Persistant
import Game.File.Base

cycleClamp :: Int -> Int -> Int -> Int
cycleClamp x min max
    | x < min = max
    | x > max = min
    | otherwise = x 

basicSelectorRule maxnr (EventKey (Char 's') Up _ _) ms@MenuState{selector = s} = ms{selector = cycleClamp (s + 1) 0 maxnr}
basicSelectorRule maxnr (EventKey (Char 'w') Up _ _) ms@MenuState{selector = s} = ms{selector = cycleClamp (s - 1) 0 maxnr}
basicSelectorRule _ _ ms = ms

highScoreIO oldPD = do
    raw <- readFile "HSCORE.txt"
    let prevHS = read raw
    let score = getInt oldPD "score"
    let newhs = newHS prevHS score
    let np = addInt oldPD "highscore" newhs
    --HIDIOUS thing to close the file so we dont get locking problems...
    --from: https://ianthehenry.com/2016/3/9/lazy-io/
    seq (length raw) (return ()) 
    writeFile "HSCORE.txt" $ show newhs
    return np

newHS prev Nothing = prev
newHS prev (Just x) = max prev x

updateHSLabel Label{msg = m, labelPos = p, labelUpdate = u} oldPD _ = makeLabelF ms p Center u
    where
        ms = case (getInt oldPD "highscore") of
            Nothing -> "highscore - error!"
            Just x -> "highscore - " ++ (show x)
updateHSLabel l _ _ = l