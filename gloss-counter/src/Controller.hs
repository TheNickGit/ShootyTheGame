-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

--leftovers TODO
--randomNumber <- randomIO

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gs
  | (not $ spritesLoaded gs)
  = -- Load Sprites first if not load
    do spr <- initSprites
       return $ gs {sprites = spr, elapsedTime = 0}
  | elapsedTime gs + secs > nO_SECS_BETWEEN_CYCLES
  = -- We update the game state
    do 
       return $ (updatePlacements . updateAnimations) $ gs { playerBullets = despawnPBs $ playerBullets gs} -- gs'
  | otherwise
  = -- Just update the elapsed time
    return $ gs { elapsedTime = elapsedTime gs + secs }

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gs = return (inputKey e gs)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (SpecialKey KeySpace) Down _ _) gs
  = -- If the user presses spacebar, spawn bullet.
    let pos       = position $ playerShip gs in
    gs { playerBullets = newBullet pos : playerBullets gs}
inputKey (EventKey key keystate _ _) gs
  = -- If the user releases movement key, stop moving in that direction
    let ps = playerShip gs in
    let pos = position ps in
    let curIn = inputCurrentDirection ps key keystate in
    let dir = directions ps in
    let mvt = inputMovement dir curIn in
    let newps = ps { placementPS = (pos, mvt), currentDirection = curIn } in
    gs { playerShip = newps }
inputKey _ gs = gs -- Otherwise keep the same

inputMovement :: MovementPlus -> (Bool, Bool, Bool, Bool) -> Movement
inputMovement ((dirxpos, dirxneg),(dirypos, diryneg)) (b1,b2,b3,b4) = let mvtxpos = (if b1 then dirxpos else 0) in
                                                                      let mvtxneg = (if b2 then dirxneg else 0) in
                                                                      let mvtypos = (if b3 then dirypos else 0) in
                                                                      let mvtyneg = (if b4 then diryneg else 0) in
                                                                        (mvtxpos + mvtxneg, mvtypos + mvtyneg)

inputCurrentDirection :: PlayerShip -> Key -> KeyState -> (Bool, Bool, Bool, Bool)
inputCurrentDirection ps key keyState =
  let (right,left,up,down) = currentDirection ps in
  case keyState of
  Down -> case key of
    (Char 'd') -> (True,left,up,down)
    (Char 'a') -> (right,True,up,down)
    (Char 'w') -> (right,left,True,down)
    (Char 's') -> (right,left,up,True)
    _          -> (right,left,up,down)
  Up -> case key of
    (Char 'd') -> (False,left,up,down)
    (Char 'a') -> (right,False,up,down)
    (Char 'w') -> (right,left,False,down)
    (Char 's') -> (right,left,up,False)
    _          -> (right,left,up,down)
