module Lib (
    start
) where

import Constants

import Controller
import Model
import View

import Graphics.Gloss.Interface.Pure.Game

window :: Display
window = InWindow gameName (width, height) (offset, offset)

start :: IO ()
start = play window background fps initialState render input updateGame