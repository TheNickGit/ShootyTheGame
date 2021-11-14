-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view4 :: GameState -> IO Picture
view4 gs
  | elapsedTime gs >= 0
  = do
    let sprs = sprites gs
    let pic = (view sprs) $ playerShip gs
    return pic
  | otherwise
  = return Blank

viewMain :: GameState -> IO Picture
viewMain gs = do
              let sprs = sprites gs
              return $ case sprs of
                NotLoaded -> Blank
                _         -> pictures $ views sprs gs [] --pictures $ views sprs gs $ viewAllCollision gs []