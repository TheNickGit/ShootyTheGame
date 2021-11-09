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
                           , sprites        :: Sprites
                           }
instance Placeables GameState where
  updatePlacements gs = gs { playerShip = updatePlacement $ playerShip gs
                           , playerBullets = updatePlacements $ playerBullets gs
                           }
instance Viewables GameState where
  views sprs gs = let addPS  = views sprs (playerShip gs) in
                  let addPBs = views sprs (playerBullets gs) in
                    addPBs . addPS

initialState :: GameState
initialState = let ps  = PlayerShip ((0,0),(0,0)) (1,1) ((22,-20),(15,-15)) (False,False,False,False) (Index 0) in
               let pbs = [] in
                 GameState (-1) ps pbs NotLoaded


------------------------------------------------------------------------------------------------------------------------------------------------------------
--Spritesheets
initSprites :: IO Sprites
initSprites = do
  psSpr   <- loadBMP "PlayerShip.bmp"
  pbSpr   <- loadBMP "PlayerBullet.bmp"
  bSpr    <- loadBMP "Wiegel.bmp"
  elSpr   <- loadBMP "Wiegel.bmp"
  eseSpr  <- loadBMP "Wiegel.bmp"
  eswSpr  <- loadBMP "Wiegel.bmp"
  ebugSpr <- loadBMP "Wiegel.bmp"
  lSpr    <- loadBMP "Wiegel.bmp"
  mSpr    <- loadBMP "Wiegel.bmp"
  bgSpr   <- loadBMP "Wiegel.bmp"
  smSpr   <- loadBMP "Wiegel.bmp"
  putStrLn "initSprites called"
  return $ SpritesState psSpr pbSpr bSpr elSpr eseSpr eswSpr ebugSpr lSpr mSpr bgSpr smSpr

spritesLoaded :: GameState -> Bool
spritesLoaded gs = case sprites gs of
  NotLoaded -> False
  _         -> True

data Sprites = NotLoaded |
  SpritesState { playerShipSprite     :: Picture
               , playerBulletSprite   :: Picture
               , bulletSprite         :: Picture
               , enemyLaserSprite     :: Picture
               , enemySeekerSprite    :: Picture
               , enemySwarmSprite     :: Picture
               , enemyBugSprite       :: Picture
               , laserSprite          :: Picture
               , meteorSprite         :: Picture
               , backgroundSprite     :: Picture
               , smokeSprite          :: Picture
               }

data Animation = Invisible | Index Int

--Values
type MovementPlus = ((Float, Float),(Float, Float))
type Movement     = Vector
type Placement    = (Vector, Movement)
type ScorePoints  = Int

--Entities
--type Entities = PlayerBullet --Positionable, despawnable objects. TODO add stuff with |

------------------------------------------------------------------------------------------------------------------------------------------------------------
--PlayerShip

data PlayerShip = PlayerShip { placementPS      :: Placement
                             , sizePS           :: Vector
                             , directions       :: MovementPlus
                             , currentDirection :: (Bool, Bool, Bool, Bool)
                             , spriteStatePS    :: Animation
                             }
instance Placeable PlayerShip where
  placement = placementPS
  updatePlacement ps = ps {placementPS = stayInBounds $ nextPosition $ placement ps }
instance Positionable PlayerShip where
  position = fst . placement
instance Moveable PlayerShip where
  movement = snd . placement
instance Sizeable PlayerShip where
  size = sizePS
instance Collideable PlayerShip where
  collision = collisionPB
instance Viewable PlayerShip where
  view sprs ps = case spriteStatePS ps of
    Invisible -> Blank
    Index inx -> let spr = playerShipSprite sprs in
      translateV (position ps) $ scaleV (size ps) $ spr
instance Viewables PlayerShip where
  views sprs ps pics = view sprs ps : pics

stayInBounds :: Placement -> Placement
stayInBounds = undefined

------------------------------------------------------------------------------------------------------------------------------------------------------------
--PlayerBullets

type PlayerBullets = [PlayerBullet]
instance Placeables a => Placeables [a] where
  updatePlacements = map updatePlacements
instance Viewables a => Viewables [a] where
  views sprs vs pics = foldr (views sprs) pics vs

despawnPBs :: PlayerBullets -> PlayerBullets
despawnPBs = filter toNotDespawn


data PlayerBullet  = PlayerBullet { placementPB   :: Placement
                                  , sizePB        :: Vector
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
instance Viewable PlayerBullet where
  view sprs pb = case spriteStatePB pb of
    Invisible -> Blank
    Index inx -> let spr = playerBulletSprite sprs in
      translateV (position pb) $ scaleV (size pb) $ spr
instance Viewables PlayerBullet where
  views sprs pb pics = view sprs pb : pics

newBullet :: Vector -> PlayerBullet
newBullet pos = PlayerBullet (pos,(25,0)) (1,1) (Index 0)

------------------------------------------------------------------------------------------------------------------------------------------------------------
--Enemies
--Enemy species: Laser, Seeker, Swarm with bugs
type Enemies = [Enemy]
data Enemy = Laser    { placementEn   :: Placement
                      , sizeEn        :: Vector
                      , spriteStateEn :: Animation
                      , scoreValueEn  :: ScorePoints
                      }
           | Seeker   { placementEn   :: Placement
                      , sizeEn        :: Vector
                      , spriteStateEn :: Animation
                      , scoreValueEn  :: ScorePoints
                      }
           | Swarm    { placementEn   :: Placement
                      , sizeEn        :: Vector
                      , spriteStateEn :: Animation
                      , scoreValueEn  :: ScorePoints
                      }
           | Bug      { placementEn   :: Placement
                      , sizeEn        :: Vector
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
instance Viewable Enemy where
  view sprs en = case spriteStateEn en of
    Invisible -> Blank
    Index inx -> let spr = enemySprite en sprs in
      translateV (position en) $ scaleV (size en) $ spr
instance Viewables Enemy where
  views sprs en pics = view sprs en : pics

enemySprite :: Enemy -> Sprites -> Picture
enemySprite en = case enemyType en of
  LaserType   -> enemyLaserSprite
  SeekerType  -> enemySeekerSprite
  SwarmType   -> enemySwarmSprite
  BugType     -> enemyBugSprite

--Prevent rewriting when variable is added later on
data EnemyType = LaserType | SeekerType | SwarmType | BugType
enemyType :: Enemy -> EnemyType
enemyType (Laser  _ _ _ _) = LaserType
enemyType (Seeker _ _ _ _) = SeekerType
enemyType (Swarm  _ _ _ _) = SwarmType
enemyType (Bug    _ _ _ _) = BugType


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

data Collision = Shapes
type Shape = [Shape]
data Shape = Rectang Vector Vector
           | Triang Vector Vector Vector
           | Elipse Vector Vector
class Collideable a where
  collision :: a -> Collision

doObjectsCollide :: Collideable a -> Collideable b -> Bool

doesCollide :: Collision -> Collision -> Bool
doesCollide = undefined

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