# fp-pacman

Retro pacman game made haskell by Egor Dmitriev and Cody Bloemhard.

## Video

- [![click this image](https://img.youtube.com/vi/DlifcJ4cexw/0.jpg)](https://www.youtube.com/watch?v=DlifcJ4cexw)
- [or click this link](https://youtu.be/DlifcJ4cexw)

## How to play

Try to eat all the the dots. 
Avoid the ghosts.
If you eat a cherry(big dot) you can eat ghosts for a while.

## Controls

- W/S to select menu items.
- Enter to activate a menu item.
- W/A/S/D to move pacman.
- P to pause the game.

## Known errors

### GLUT
 
Make shure you have glut installed.
``pacman -S glut`` on arch or equivalent.
``fp-pacman-exe.EXE: user error (unknown GLUT entry glutInit)``

To fix it, move freeglut.dll [from bin folder](http://files.transmissionzero.co.uk/software/development/GLUT/freeglut-MinGW.zip) to your system32.
Or if on linux, install glut library.

### SDL2

``--  While building package sdl2-mixer-1.1.0 using:
[lots of paths]
Configuring sdl2-mixer-1.1.0...
Cabal-simple_mPHDZzAJ_2.2.0.1_ghc-8.4.3: The pkg-config package 'SDL2_mixer'
version >=2.0.0 is required but it could not be found.``

To fix, install sdl2: ``pacman -S sdl2 sdl2_mixer``, or whatever your package manager is.
It should also install mpg123, opusfile.
