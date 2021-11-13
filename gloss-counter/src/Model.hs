-- | This module contains the data types
--   which represent the state of the game
module Model where

import Graphics.Gloss

screenSize :: (Int, Int)
screenSize = (1520, 855)

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 1/60

------------------------------------------------------------------------------------------------------------------------------------------------------------
--GameState

data GameState = GameState { elapsedTime    :: Float
                           , playerShip     :: PlayerShip
                           , playerBullets  :: PlayerBullets
                           , enemies        :: Enemies
                           , sprites        :: Sprites
                           }
instance Placeables GameState where
  updatePlacements gs = gs { playerShip = updatePlacement $ playerShip gs
                           , playerBullets = updatePlacements $ playerBullets gs
                           }
instance Viewables GameState where
  views sprs gs = let addPS  = views sprs (playerShip gs) in
                  let addPBs = views sprs (playerBullets gs) in
                  let addEns = views sprs (enemies gs) in
                    addEns . addPBs . addPS

initialState :: GameState
initialState = let ps  = newPlayerShip in
                 GameState (-1) ps [] [(Laser ((500,0),(0,0)) (0.2,0.2) [RectangleF (-20,-20) (20,20)] (Index 0) 10)] NotLoaded
                  

viewCollisionGS :: GameState -> Picture
viewCollisionGS gs =  let psCV = viewCollisionOf $ playerShip gs in
                      -- let pbCV = viewCollisionOf $ head $ playerBullets gs in
                      -- let enCV = viewCollisionOf $ head $ enemies gs in
                        pictures [psCV] -- , pbCV, enCV
                        
------------------------------------------------------------------------------------------------------------------------------------------------------------
--Spritesheets
initSprites :: IO Sprites
{-
initSprites = do
  psSpr   <- loadPSSprites
  pbSpr   <- loadPBSprites               -- loadBMP "PlayerBulletA.bmp"
  --bSpr    <- loadBMP "Wiegel.bmp"
  elSpr   <- loadELSprites
  --eseSpr  <- loadBMP "Wiegel.bmp"
  --eswSpr  <- loadBMP "Wiegel.bmp"
  --ebugSpr <- loadBMP "Wiegel.bmp"
  --lSpr    <- loadBMP "Wiegel.bmp"
  --mSpr    <- loadBMP "Wiegel.bmp"
  --bgSpr   <- loadBMP "Wiegel.bmp"
  --smSpr   <- loadBMP "Wiegel.bmp"
  --putStrLn "initSprites called"
  --return $ SpritesState psSpr pbSpr bSpr elSpr eseSpr eswSpr ebugSpr lSpr mSpr bgSpr smSpr
  return $ SpritesState psSpr pbSpr elSpr
-}
initSprites = do
   psSpr <- loadPSSprites
   pbSpr <- loadPBSprites
   elSpr <- loadELSprites
   return (SpritesState psSpr pbSpr elSpr)

spritesLoaded :: GameState -> Bool
spritesLoaded gs = case sprites gs of
  NotLoaded -> False
  _         -> True

data Sprites = NotLoaded |
  SpritesState { playerShipSprite     :: [Picture]
               , playerBulletSprite   :: [Picture]
               --, bulletSprite         :: Picture
               , enemyLaserSprite     :: [Picture]
               --, enemySeekerSprite    :: Picture
               --, enemySwarmSprite     :: Picture
               --, enemyBugSprite       :: Picture
               --, laserSprite          :: Picture
               --, meteorSprite         :: Picture
               --, backgroundSprite     :: Picture
               --, smokeSprite          :: Picture
               }

data Animation = Invisible | Index Int

------------------------------------------------------------------------------------------------------------------------------------------------------------
--Values
type MovementPlus = ((Float, Float),(Float, Float))
type Movement     = Vector
type Placement    = (Vector, Movement)
type ScorePoints  = Int

instance (Num a, Num b) => Num (a,b) where
  (x,y) + (u,v) = (x+u,y+v)
  (x,y) - (u,v) = (x-u,y-v)
  (x,y) * (u,v) = (x*u,y*v)
  abs (x,y)     = (abs x, abs y)
  signum (x,y)  = (signum x, signum y)
  fromInteger i = (fromInteger i, 0)

------------------------------------------------------------------------------------------------------------------------------------------------------------
--PlayerShip

data PlayerShip = PlayerShip { placementPS      :: Placement
                             , sizePS           :: Vector
                             , directions       :: MovementPlus
                             , currentDirection :: (Bool, Bool, Bool, Bool)
                             , collisionPS      :: Collision
                             , spriteStatePS    :: Animation
                             }
instance Placeable PlayerShip where
  placement = placementPS
  updatePlacement ps = ps {placementPS = nextPosition $ placement ps } --TODO stayInBounds (collision ps) $ 
instance Positionable PlayerShip where
  position = fst . placement
instance Moveable PlayerShip where
  movement = snd . placement
instance Sizeable PlayerShip where
  size = sizePS
instance Collideable PlayerShip where
  collision = collisionPS
instance Viewable PlayerShip where
  view sprs ps = case spriteStatePS ps of
    Invisible -> Blank
    Index inx -> let spr = playerShipSprite sprs in
      translateV (position ps) $ scaleV (size ps) $ spr !! (inx `mod` length spr)
instance Viewables PlayerShip where
  views sprs ps pics = view sprs ps : pics

-- Load the sprite list of the player ship.
loadPSSprites :: IO [Picture]
loadPSSprites = sequence [ loadBMP "PlayerShip.bmp" ]

newPlayerShip :: PlayerShip
newPlayerShip = PlayerShip ((0,0),(0,0)) (1,1) ((22,-20),(15,-15)) (False,False,False,False) [RectangleF (-20,-20) (20,20)] (Index 0)

{-
stayInBounds :: Collision -> Placement -> Placement
stayInBounds (Triangle vx vy vz) =
  let sx = (fromIntegral $ fst screenSize :: Float) / 2 in
  let sy = (fromIntegral $ snd screenSize :: Float) / 2 in
  let 
  let correctionX = - maximum [(fst vx),(fst vy),(fst vz),sx] - minimum[(fst vx),(fst vy),(fst vz),(-sx)]
  let correctionY = - maximum [(snd vx),(snd vy),(snd vz),sy] - minimum[(snd vx),(snd vy),(snd vz),(-sy)]
-}

------------------------------------------------------------------------------------------------------------------------------------------------------------
--PlayerBullets

type PlayerBullets = [PlayerBullet]
instance Placeables a => Placeables [a] where
  updatePlacements = map updatePlacements
instance Viewables a => Viewables [a] where
  views sprs vs pics = foldr (views sprs) pics vs

despawnPBs :: PlayerBullets -> PlayerBullets
despawnPBs = filter toNotDespawn

-- Load the sprite list of the player bullets.
loadPBSprites :: IO [Picture]
loadPBSprites = sequence [
  loadBMP "Playerbullets\\PlayerBullet0.bmp"
  ]

data PlayerBullet  = PlayerBullet { placementPB   :: Placement
                                  , sizePB        :: Vector
                                  , collisionPB   :: Collision
                                  , spriteStatePB :: Animation
                                  }
instance Placeable PlayerBullet where
  placement = placementPB
  updatePlacement pb = pb {placementPB = nextPosition $ placement pb }
instance Placeables PlayerBullet where
  updatePlacements = updatePlacement
instance Positionable PlayerBullet where
  position = fst . placement
instance Moveable PlayerBullet where
  movement = snd . placement
instance Sizeable PlayerBullet where
  size = sizePB
instance Collideable PlayerBullet where
  collision = collisionPB
instance Viewable PlayerBullet where
  view sprs pb = case spriteStatePB pb of
    Invisible -> Blank
    Index inx -> let spr = playerBulletSprite sprs in
      translateV (position pb) $ scaleV (size pb) $ spr !! (inx `mod` length spr)
instance Viewables PlayerBullet where
  views sprs pb pics = view sprs pb : pics

newBullet :: Vector -> PlayerBullet
newBullet pos = PlayerBullet (pos,(2.5,0)) (1,1) [RectangleF (-2.0,-2.0) (2.0,2.0)] (Index 0)

------------------------------------------------------------------------------------------------------------------------------------------------------------
--Enemies
--Enemy species: Laser, Seeker, Swarm with bugs
type Enemies = [Enemy]
data Enemy = Laser    { placementEn   :: Placement
                      , sizeEn        :: Vector
                      , collisionEn   :: Collision
                      , spriteStateEn :: Animation
                      , scoreValueEn  :: ScorePoints
                      }
           | Seeker   { placementEn   :: Placement
                      , sizeEn        :: Vector
                      , collisionEn   :: Collision
                      , spriteStateEn :: Animation
                      , scoreValueEn  :: ScorePoints
                      }
           | Swarm    { placementEn   :: Placement
                      , sizeEn        :: Vector
                      , collisionEn   :: Collision
                      , spriteStateEn :: Animation
                      , scoreValueEn  :: ScorePoints
                      }
           | Bug      { placementEn   :: Placement
                      , sizeEn        :: Vector
                      , collisionEn   :: Collision
                      , spriteStateEn :: Animation
                      , scoreValueEn  :: ScorePoints
                      }

instance Placeable Enemy where
  placement = placementEn
  updatePlacement en = en {placementEn = nextPosition $ placement en }
instance Placeables Enemy where
  updatePlacements = updatePlacement
instance Positionable Enemy where
  position = fst . placement
instance Moveable Enemy where
  movement = snd . placement
instance Sizeable Enemy where
  size = sizeEn
instance Collideable Enemy where
  collision = collisionEn
instance Viewable Enemy where
  view sprs en = case spriteStateEn en of
    Invisible -> Blank
    Index inx -> let spr = enemySprite en sprs in
      translateV (position en) $ scaleV (size en) $ spr !! (inx `mod` length spr)
instance Viewables Enemy where
  views sprs en pics = view sprs en : pics

enemySprite :: Enemy -> Sprites -> [Picture]
enemySprite en = case enemyType en of
  LaserType   -> enemyLaserSprite
  --SeekerType  -> enemySeekerSprite
  --SwarmType   -> enemySwarmSprite
  --BugType     -> enemyBugSprite

-- Load the sprite list of the enemy laser.
loadELSprites :: IO [Picture]
loadELSprites = sequence [ loadBMP "Wiegel.bmp" ]

--Prevent rewriting when variable is added later on
data EnemyType = LaserType | SeekerType | SwarmType | BugType
enemyType :: Enemy -> EnemyType
enemyType (Laser  _ _ _ _ _) = LaserType
enemyType (Seeker _ _ _ _ _) = SeekerType
enemyType (Swarm  _ _ _ _ _) = SwarmType
enemyType (Bug    _ _ _ _ _) = BugType


------------------------------------------------------------------------------------------------------------------------------------------------------------
--Type classes and type functions
class Positionable a where
  position :: a -> Vector

class Moveable a where
  movement :: a -> Movement

class (Positionable a, Moveable a) => Placeable a where
  placement :: a -> Placement
  updatePlacement :: a -> a
class Placeables a where
  updatePlacements :: a -> a

class Sizeable a where
  size :: a -> Vector

class (Positionable a) => Viewable a where
  view :: Sprites -> a -> Picture
class Viewables a where
  views :: Sprites -> a -> [Picture] -> [Picture]

class Despawnables a where
  toDespawn :: a -> a

--TODO
type Collision = [RectangleF]
data RectangleF = RectangleF Vector Vector  -- Bottom Left Corner and Top Right Corner
class Positionable a => Collideable a where
  collision :: a -> Collision

objectsCollide :: (Collideable a, Collideable b) => a -> b -> Bool
objectsCollide obj1 obj2 =  let col1 = collision obj1 in
                            let col2 = collision obj2 in
                            let pos1 = position obj1 in
                            let pos2 = position obj2 in
                              doesCollide pos1 col1 pos2 col2

doesCollide :: Vector -> Collision -> Vector -> Collision -> Bool
doesCollide _    []   _    _    = False
doesCollide _    _    _    []   = False
doesCollide pos1 (rec1:col1) pos2 col2 = any (rectangleCollide pos1 rec1 pos2) col2 || doesCollide pos1 col1 pos2 col2

rectangleCollide :: Vector -> RectangleF -> Vector -> RectangleF -> Bool
rectangleCollide pos1 (RectangleF vec11 vec12) pos2 (RectangleF vec21 vec22) =
  let (x11,y11) = vec11 + pos1 in           --(Bottom, Left)
  let (x12,y12) = vec12 + pos1 in           --(Top, Right)
  let (x21,y21) = vec21 + pos2 in           --(Bottom, Left)
  let (x22,y22) = vec22 + pos2 in           --(Top, Right)
    not (x12 < x21 || x11 > x22 || y12 < y21 || y11 > y22)

viewCollisionOf :: (Viewable a, Collideable a) => a -> Picture
viewCollisionOf x = let pos = position x in
                    let col = collision x in
                      translateV pos $ viewCollision col

viewCollision :: Collision -> Picture
viewCollision col = pictures $ map viewRectangle col

viewRectangle :: RectangleF -> Picture
viewRectangle (RectangleF (x1,x2) (y1,y2))  = Color red $ polygon [(x1,x2), (x1,y2), (y1,y2), (y1,x2), (x1,x2)]

--Out of bounds despawning
threshDespawn :: Float
threshDespawn = 10
toNotDespawn :: Positionable a => a -> Bool
toNotDespawn x =
  let trs = threshDespawn in
  let pos = position x in
  let sx = trs + (fromIntegral $ fst screenSize :: Float) / 2 in
  let sy = trs + (fromIntegral $ snd screenSize :: Float) / 2 in
  (fst pos <  sx) &&
  (fst pos > -sx) &&
  (snd pos > -sy) &&
  (snd pos <  sy)

--Placement
--TODO turn into instance perhaps, change data type into something like game state, with functions, safes on typing
nextPosition :: Placement -> Placement
nextPosition ((posx, posy), mvt@((mvtx), (mvty))) = ((posx+mvtx, posy+mvty),mvt)

--View
--translate, scale but with vectors
translateV  :: Vector -> Picture -> Picture
translateV  (fl1, fl2) = translate  fl1 fl2
scaleV      :: Vector -> Picture -> Picture
scaleV      (fl1, fl2) = scale      fl1 fl2