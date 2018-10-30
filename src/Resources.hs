module Resources where
import Engine.Graphics.Base
import Engine.Audio.Sound
import Engine.Audio.Loading
import Constants

-- Levels
levelClassic = resourceDir ++ "levels/classic.txt"

-- Sprites
spritePacmanDie   = loadAnimatedSprite "pacman/die" 11 Single spriteInterval
spritePacmanStill = loadStaticSprite "pacman/up"
spritePacmanUp    = loadAnimatedSprite "pacman/up" 2 Repeating spriteInterval
spritePacmanDown  = loadAnimatedSprite "pacman/down" 2 Repeating spriteInterval
spritePacmanLeft  = loadAnimatedSprite "pacman/left" 2 Repeating spriteInterval
spritePacmanRight = loadAnimatedSprite "pacman/right" 2 Repeating spriteInterval

spriteBlinkyStill = loadStaticSprite "ghosts/blinky/up"
spriteBlinkyUp    = loadAnimatedSprite "ghosts/blinky/up" 1 Repeating spriteInterval
spriteBlinkyDown  = loadAnimatedSprite "ghosts/blinky/down" 1 Repeating spriteInterval
spriteBlinkyLeft  = loadAnimatedSprite "ghosts/blinky/left" 1 Repeating spriteInterval
spriteBlinkyRight = loadAnimatedSprite "ghosts/blinky/right" 1 Repeating spriteInterval

spriteClydeStill  = loadStaticSprite "ghosts/clyde/up"
spriteClydeUp     = loadAnimatedSprite "ghosts/clyde/up" 1 Repeating spriteInterval
spriteClydeDown   = loadAnimatedSprite "ghosts/clyde/down" 1 Repeating spriteInterval
spriteClydeLeft   = loadAnimatedSprite "ghosts/clyde/left" 1 Repeating spriteInterval
spriteClydeRight  = loadAnimatedSprite "ghosts/clyde/right" 1 Repeating spriteInterval

spriteInkyStill   = loadStaticSprite "ghosts/inky/up"
spriteInkyUp      = loadAnimatedSprite "ghosts/inky/up" 1 Repeating spriteInterval
spriteInkyDown    = loadAnimatedSprite "ghosts/inky/down" 1 Repeating spriteInterval
spriteInkyLeft    = loadAnimatedSprite "ghosts/inky/left" 1 Repeating spriteInterval
spriteInkyRight   = loadAnimatedSprite "ghosts/inky/right" 1 Repeating spriteInterval

spritePinkyStill  = loadStaticSprite "ghosts/pinky/up"
spritePinkyUp     = loadAnimatedSprite "ghosts/pinky/up" 1 Repeating spriteInterval
spritePinkyDown   = loadAnimatedSprite "ghosts/pinky/down" 1 Repeating spriteInterval
spritePinkyLeft   = loadAnimatedSprite "ghosts/pinky/left" 1 Repeating spriteInterval
spritePinkyRight  = loadAnimatedSprite "ghosts/pinky/right" 1 Repeating spriteInterval

spriteEyesUp      = loadStaticSprite "ghosts/eyes/up"
spriteEyesDown    = loadStaticSprite "ghosts/eyes/down"
spriteEyesLeft    = loadStaticSprite "ghosts/eyes/left"
spriteEyesRight   = loadStaticSprite "ghosts/eyes/right"

spriteScatter  = loadAnimatedSprite "ghosts/scatter" 1 Repeating spriteInterval
spriteScatterEnding  = loadAnimatedSprite "ghosts/scatter" 3 Repeating spriteInterval

spriteFont = frames (loadAnimatedSprite "font" 40 Repeating spriteInterval)
fontMapping = ['a' .. 'z'] ++ "!C" ++ ['0'..'9'] ++ "/-" ++ ['"']

-- Powerups
spritePowerupPacDot      = loadStaticSpriteFile "powerups/pacdot"
spritePowerupPowerPellet = loadStaticSpriteFile "powerups/power_pellet"
spritePowerupCherry      = loadStaticSpriteFile "powerups/01"

-- Tiles. (Pls send help)
spriteTileSCornerLD        = loadStaticSpriteFile "tiles/01"
spriteTileSCornerRD        = loadStaticSpriteFile "tiles/02"
spriteTileSCornerLU        = loadStaticSpriteFile "tiles/05"
spriteTileSCornerRU        = loadStaticSpriteFile "tiles/06"

spriteTileSStraightR       = loadStaticSpriteFile "tiles/03"
spriteTileSStraightL       = loadStaticSpriteFile "tiles/04"
spriteTileSStraightU       = loadStaticSpriteFile "tiles/11"
spriteTileSStraightD       = loadStaticSpriteFile "tiles/13"

spriteTileSSplitUDL         = loadStaticSpriteFile "tiles/07"
spriteTileSSplitUDR         = loadStaticSpriteFile "tiles/08"
spriteTileSSplitDUL         = loadStaticSpriteFile "tiles/09"
spriteTileSSplitDUR         = loadStaticSpriteFile "tiles/10"
spriteTileSSplitRLD         = loadStaticSpriteFile "tiles/43"
spriteTileSSplitLRD         = loadStaticSpriteFile "tiles/44"

spriteTileFStraightD       = loadStaticSpriteFile "tiles/15"
spriteTileFStraightU       = loadStaticSpriteFile "tiles/21"
spriteTileFStraightL       = loadStaticSpriteFile "tiles/25"
spriteTileFStraightR       = loadStaticSpriteFile "tiles/26"

spriteTileFCornerLUUR      = loadStaticSpriteFile "tiles/17"
spriteTileFCornerRUUL      = loadStaticSpriteFile "tiles/18"
spriteTileFCornerLDDR      = loadStaticSpriteFile "tiles/19"
spriteTileFCornerRDDL      = loadStaticSpriteFile "tiles/20"
spriteTileFCornerLDDL      = loadStaticSpriteFile "tiles/23"
spriteTileFCornerRDDR      = loadStaticSpriteFile "tiles/24"
spriteTileFCornerLUUL      = loadStaticSpriteFile "tiles/27"
spriteTileFCornerRUUR      = loadStaticSpriteFile "tiles/28"

spriteTileCCornerLD        = loadStaticSpriteFile "tiles/29"
spriteTileCCornerRD        = loadStaticSpriteFile "tiles/30"
spriteTileCCornerLU        = loadStaticSpriteFile "tiles/31"
spriteTileCCornerRU        = loadStaticSpriteFile "tiles/32"
spriteTileCEndL            = loadStaticSpriteFile "tiles/33"
spriteTileCEndR            = loadStaticSpriteFile "tiles/34"

spriteTileDoor            = loadStaticSpriteFile "tiles/45"

-- Audio
data Sounds = Sounds {
                soundDeath1 :: Sound,
                soundDeath2 :: Sound,
                soundDeath3 :: Sound,
                soundExtraMan :: Sound,
                soundFruit :: Sound,
                soundGhostEat1 :: Sound,
                soundGhostEat2 :: Sound,
                soundGhostEat3 :: Sound,
                soundInsertCoin :: Sound,
                soundIntermission :: Sound,
                soundIntro :: Sound,
                soundLargePelletBlip :: Sound,
                soundLargePelletLoop :: Sound,
                soundMunchA :: Sound,
                soundMunchB :: Sound,
                soundSirenFast :: Sound,
                soundSirenMedium :: Sound,
                soundSirenSlow :: Sound
              }

loadSounds :: IO Sounds
loadSounds = do
    soundDeath1          <- loadSound "sounds/death_1" 4 100
    soundDeath2          <- loadSound "sounds/death_2" 4 100
    soundDeath3          <- loadSound "sounds/death_3" 4 100
    soundExtraMan        <- loadSound "sounds/extra_man" 3 100
    soundFruit           <- loadSound "sounds/fruit" 1 100
    soundGhostEat1       <- loadSound "sounds/ghost_eat_1" 2 100
    soundGhostEat2       <- loadSound "sounds/ghost_eat_2" 2 100
    soundGhostEat3       <- loadSound "sounds/ghost_eat_3" 2 100
    soundInsertCoin      <- loadSound "sounds/insert_coin" 3 100
    soundIntermission    <- loadSound "sounds/intermission" 4 100
    soundIntro           <- loadSound "sounds/intro" 4 100
    soundLargePelletBlip <- loadSound "sounds/large_pellet_blip" 5 100
    soundLargePelletLoop <- loadSound "sounds/large_pellet_loop" 4 100
    soundMunchA          <- loadSound "sounds/munch_a" 5 100
    soundMunchB          <- loadSound "sounds/munch_b" 5 100
    soundSirenFast       <- loadSound "sounds/siren_fast" 4 100
    soundSirenMedium     <- loadSound "sounds/siren_medium" 4 100
    soundSirenSlow       <- loadSound "sounds/siren_slow" 4 100
    return (Sounds soundDeath1 soundDeath2 soundDeath3 soundExtraMan soundFruit soundGhostEat1 soundGhostEat2
      soundGhostEat3 soundInsertCoin soundIntermission soundIntro soundLargePelletBlip soundLargePelletLoop soundMunchA
      soundMunchB soundSirenFast soundSirenMedium soundSirenSlow)