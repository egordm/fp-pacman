module Game.Menu.GameOverMenu (
    gameOverMenu
) where

import Engine.Base
import Graphics.Gloss.Game
import Game.UI.Base
import Game.Context.SwitchRoom
import Game.Context.Room
import Game.Menu.MenuShared
import Game.Context.Persistant
import Game.File.Base

replayButtonF (EventKey (SpecialKey KeyEnter) Up _ _) b = b{itemSwitch = RoomSwitch "classic" ReloadRoom}
replayButtonF _ b = b

mainButtonF (EventKey (SpecialKey KeyEnter) Up _ _) b = b{itemSwitch = RoomSwitch "main" ReloadRoom}
mainButtonF _ b = b

updateScoreLabelF Label{msg = m, labelPos = p, labelUpdate = u} oldPD _ = makeLabelF ms p Center u
    where
        ms = case (getInt oldPD "score") of
            Nothing -> "score - error!"
            Just x -> "score - " ++ (show x)
updateScoreLabelF l _ _ = l

updateHSLabel Label{msg = m, labelPos = p, labelUpdate = u} oldPD _ = makeLabelF ms p Center u
    where
        ms = case (getInt oldPD "highscore") of
            Nothing -> "highscore - error!"
            Just x -> "highscore - " ++ (show x)
updateHSLabel l _ _ = l

iof oldPD = do
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

uiElements = [
    makeLabel "game over!" (Coordinate 0 (-200)) Center,
    makeLabelF "score - error" (Coordinate 0 (-160)) Center updateScoreLabelF,
    makeLabelF "highscore - error" (Coordinate 0 (-120)) Center updateHSLabel,
    makeButton "replay" "-replay-" 0 replayButtonF (Coordinate 0 0),
    makeButton "main menu" "-main menu-" 1 mainButtonF (Coordinate 0 40)]

gameOverMenu = makeMenuF uiElements [basicSelectorRule 1] iof
