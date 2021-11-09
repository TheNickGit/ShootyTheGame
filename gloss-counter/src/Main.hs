module Main where

import Controller
import Model
import View

import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = playIO (InWindow "Counter" screenSize (0, 0)) -- Or FullScreen
              black            -- Background color
              69               -- Frames per second
              initialState     -- Initial state
              viewMain         -- View function
              input            -- Event function
              step             -- Step function