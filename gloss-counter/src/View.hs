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
              let picCols = viewCollisionGS gs
              return $ case sprs of
                NotLoaded -> Blank
                _         -> pictures $ (picCols :) $ views sprs gs []


{-
viewPure :: GameState -> Picture
viewPure gstate = case infoToShow gstate of
  ShowNothing   -> blank
  ShowANumber n -> color blue (text (show n))
  ShowAChar   c -> color red (text [c])

viewPure2 :: GameState -> Picture
viewPure2 gstate = case infoToShow2 gstate of
  ShowNothing   -> blank
  ShowANumber n -> color white (text (show ((n-5) `mod` 10)))
  ShowAChar   c -> color green (text [c,' ','y','e','s'])
-}
  