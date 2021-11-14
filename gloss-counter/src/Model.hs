-- | This module contains the data types
--   which represent the state of the game
module Model where

import Graphics.Gloss
import GHC.Err (undefined)

screenSize :: (Int, Int)
screenSize = (1424, 801)

halfScreenSize :: Vector
halfScreenSize =  let sx = fromIntegral (fst screenSize) :: Float in
                  let sy = fromIntegral (snd screenSize) :: Float in
                    (sx,sy)

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 1/60

------------------------------------------------------------------------------------------------------------------------------------------------------------
--GameState
data GameState = GameState { elapsedTime    :: Float
                           , timeTillNextEnemy :: Float
                           , playerShip     :: PlayerShip
                           , playerBullets  :: PlayerBullets
                           , enemies        :: Enemies
                           , playerScore    :: ScorePoints
                           , sprites        :: Sprites
                           }
instance Placeables GameState where
  updatePlacements gs = gs { playerShip = updatePlacement $ playerShip gs
                           , playerBullets = updatePlacements $ playerBullets gs
                           , enemies = updatePlacements $ enemies gs
                           }
instance Viewables GameState where
  views sprs gs = let addPS  = views sprs (playerShip gs) in
                  let addPBs = views sprs (playerBullets gs) in
                  let addEns = views sprs (enemies gs) in
                    addEns . addPBs . addPS
  updateAnimations gs = gs { playerBullets = updateAnimations $ playerBullets gs, enemies = updateAnimations $ enemies gs }

instance ViewCollideables GameState where
  viewAllCollision gs = let addPS  = viewAllCollision (playerShip gs) in
                        let addPBs = viewAllCollision (playerBullets gs) in
                        let addEns = viewAllCollision (enemies gs) in
                          addEns . addPBs . addPS

updateMovementEnGS :: Float -> GameState -> GameState
updateMovementEnGS t gs = let ens = map (updateMovementEn t) $ enemies gs in
                          gs {enemies = ens}

updateHealthGS :: GameState -> GameState
updateHealthGS gs = gs {playerShip = ps1, playerBullets = pbs1, enemies = ens2, playerScore = pt2} where
                      ps  = playerShip gs
                      pbs = playerBullets gs
                      ens = enemies gs
                      pt = playerScore gs
                      (pt1, ps1, ens1) = updateHealth1 (pt, ps, ens)
                      (pt2, ens2, pbs1) = updateHealth2 (pt1, ens1, pbs)
                      
initialState :: GameState
initialState = let ps  = newPlayerShip in
                 GameState (-1) 10 ps [] [] 0 NotLoaded

------------------------------------------------------------------------------------------------------------------------------------------------------------
--Spritesheets
initSprites :: IO Sprites
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
               , enemyLaserSprite     :: [Picture]
               }

data Animation = Invisible | Index Int deriving (Show)

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

type Collision = [RectangleF]
data RectangleF = RectangleF Vector Vector deriving (Show)  -- Bottom Left Corner and Top Right Corner

type Health = MaybeInf Int
data MaybeInf a = NegInf | Fin a | PosInf deriving (Eq,Ord)

instance (Show a) => Show (MaybeInf a) where
  show PosInf = "inf"
  show NegInf = "-inf"
  show (Fin a) = show a

addInf :: Num a => MaybeInf a -> MaybeInf a -> MaybeInf a
addInf _ PosInf        = PosInf   --Overwrite to new value (Rightside)
addInf _ NegInf        = NegInf   --Overwrite to new value (Rightside)
addInf PosInf _        = PosInf
addInf NegInf _        = NegInf
addInf (Fin x) (Fin y) = Fin (x + y)

subInf :: Num a => MaybeInf a -> MaybeInf a -> MaybeInf a
subInf _ PosInf        = NegInf   --Overwrite to new value (Rightside)
subInf _ NegInf        = PosInf   --Overwrite to new value (Rightside)
subInf PosInf _        = PosInf
subInf NegInf _        = NegInf
subInf (Fin x) (Fin y) = Fin (x - y)

------------------------------------------------------------------------------------------------------------------------------------------------------------
--PlayerShip
data PlayerShip = PlayerShip { placementPS      :: Placement
                             , sizePS           :: Vector
                             , directions       :: MovementPlus
                             , currentDirection :: (Bool, Bool, Bool, Bool)
                             , collisionPS      :: Collision
                             , healthPS         :: Health
                             , damagePS         :: Health
                             , spriteStatePS    :: Animation
                             } deriving (Show)
instance Placeable PlayerShip where
  placement = placementPS
  updatePlacement ps = ps {placementPS = nextPosition (-70) (placement ps) } --TODO stayInBounds (collision ps) $ 
instance Positionable PlayerShip where
  position = fst . placement
instance Moveable PlayerShip where
  movement = snd . placement
instance Sizeable PlayerShip where
  size = sizePS
instance Collideable PlayerShip where
  collision = collisionPS
instance Hurtable PlayerShip where
  health        = healthPS
  getDamage hp ps = ps {healthPS = subInf (health ps) hp}
instance Painful PlayerShip where
  damage        = damagePS
instance Scoreable PlayerBullet where
  score         = (\_ -> 0)
instance Viewable PlayerShip where
  view sprs ps = case spriteStatePS ps of
    Invisible -> Blank
    Index inx -> let spr = playerShipSprite sprs in
      translateV (position ps) $ scaleV (size ps) $ spr !! (inx `mod` length spr)
  updateAnimation ps = ps { spriteStatePS = nextAnimation $ spriteStatePS ps}
instance Viewables PlayerShip where
  views sprs ps pics = view sprs ps : pics
  updateAnimations = updateAnimation
instance ViewCollideables PlayerShip where
  viewAllCollision ps pics = viewCollisionOf ps : pics

-- Load the sprite list of the player ship.
loadPSSprites :: IO [Picture]
loadPSSprites = sequence [ loadBMP "PlayerShip.bmp" ]

newPlayerShip :: PlayerShip
newPlayerShip = PlayerShip ((-500,0),(0,0)) (1,1) ((22,-20),(15,-15)) (False,False,False,False) [RectangleF (-40,-55) (-10,55), RectangleF (-10,-35) (15,35), RectangleF (-60,-25) (60,25)] (Fin 5) (PosInf) (Index 0)

------------------------------------------------------------------------------------------------------------------------------------------------------------
--PlayerBullets
type PlayerBullets = [PlayerBullet]
instance Placeables a => Placeables [a] where
  updatePlacements = map updatePlacements
instance Viewables a => Viewables [a] where
  views sprs vs pics = foldr (views sprs) pics vs
  updateAnimations = map updateAnimations
instance ViewCollideables a => ViewCollideables [a] where
  viewAllCollision vs pics = foldr (viewAllCollision) pics vs

despawnPBs :: PlayerBullets -> PlayerBullets
despawnPBs = filter toNotDespawn

-- Load the sprite list of the player bullets.
loadPBSprites :: IO [Picture]
loadPBSprites = let spritelist = map loadBMP [
                      "Playerbullets\\PlayerBullet0.bmp",
                      "Playerbullets\\PlayerBullet1.bmp",
                      "Playerbullets\\PlayerBullet2.bmp",
                      "Playerbullets\\PlayerBullet3.bmp",
                      "Playerbullets\\PlayerBullet4.bmp"
                      ]
                in sequence (spritelist ++ reverse spritelist)

data PlayerBullet  = PlayerBullet { placementPB   :: Placement
                                  , sizePB        :: Vector
                                  , collisionPB   :: Collision
                                  , healthPB      :: Health
                                  , damagePB      :: Health
                                  , spriteStatePB :: Animation
                                  } deriving (Show)
instance Placeable PlayerBullet where
  placement = placementPB
  updatePlacement pb = pb {placementPB = nextPosition 100 (placement pb) }
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
instance Hurtable PlayerBullet where
  health        = healthPB
  getDamage hp pb = pb {healthPB = subInf (health pb) hp}
instance Painful PlayerBullet where
  damage        = damagePB
instance Scoreable Enemy where
  score         = scoreValueEn
instance Viewable PlayerBullet where
  view sprs pb = case spriteStatePB pb of
    Invisible -> Blank
    Index inx -> let spr = playerBulletSprite sprs in
      translateV (position pb) $ scaleV (size pb) $ spr !! (inx `mod` length spr)
  updateAnimation pb = pb { spriteStatePB = nextAnimation $ spriteStatePB pb}
instance Viewables PlayerBullet where
  views sprs pb pics = view sprs pb : pics
  updateAnimations = updateAnimation
instance ViewCollideables PlayerBullet where
  viewAllCollision ps pics = viewCollisionOf ps : pics

newBullet :: Vector -> PlayerBullet
newBullet pos = PlayerBullet (pos,(25,0)) (1,1) [RectangleF (-7.0,-7.0) (7.0,7.0)] (Fin 1) (Fin 1) (Index 0)

------------------------------------------------------------------------------------------------------------------------------------------------------------
--Enemies
--Enemy species: Laser, Seeker, Swarm with bugs
type Enemies = [Enemy]
data Enemy = Laser    { placementEn   :: Placement
                      , amplitudeMov  :: Vector
                      , sizeEn        :: Vector
                      , collisionEn   :: Collision
                      , healthEn      :: Health
                      , damageEn      :: Health
                      , spriteStateEn :: Animation
                      , scoreValueEn  :: ScorePoints
                      } deriving (Show)
{-           | Seeker   { placementEn   :: Placement
                      , sizeEn        :: Vector
                      , collisionEn   :: Collision
                      , healthEn      :: Health
                      , damageEn      :: Health
                      , spriteStateEn :: Animation
                      , scoreValueEn  :: ScorePoints
                      }
           | Swarm    { placementEn   :: Placement
                      , sizeEn        :: Vector
                      , collisionEn   :: Collision
                      , healthEn      :: Health
                      , damageEn      :: Health
                      , spriteStateEn :: Animation
                      , scoreValueEn  :: ScorePoints
                      }
           | Bug      { placementEn   :: Placement
                      , sizeEn        :: Vector
                      , collisionEn   :: Collision
                      , healthEn      :: Health
                      , damageEn      :: Health
                      , spriteStateEn :: Animation
                      , scoreValueEn  :: ScorePoints
                      } -}

instance Placeable Enemy where
  placement = placementEn
  updatePlacement en = en {placementEn = nextPosition 50 (placement en) }
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
instance Hurtable Enemy where
  health        = healthEn
  getDamage hp en = en {healthEn = subInf (health en) hp}
instance Painful Enemy where
  damage        = damageEn
instance Viewable Enemy where
  view sprs en = case spriteStateEn en of
    Invisible -> Blank
    Index inx -> let spr = enemySprite en sprs in
      translateV (position en) $ scaleV (size en) $ spr !! (inx `mod` length spr)
  updateAnimation en = en { spriteStateEn = nextAnimation $ spriteStateEn en}
instance Viewables Enemy where
  views sprs en pics = view sprs en : pics
  updateAnimations = updateAnimation
instance ViewCollideables Enemy where
  viewAllCollision ps pics = viewCollisionOf ps : pics

enemySprite :: Enemy -> Sprites -> [Picture]
enemySprite en = case enemyType en of
  LaserType   -> enemyLaserSprite

-- Load the sprite list of the enemy laser.
loadELSprites :: IO [Picture]
loadELSprites = sequence $ map loadBMP ["Laser\\Laser0.bmp", "Laser\\Laser1.bmp", "Laser\\Laser2.bmp", "Laser\\Laser3.bmp"]

--Prevent rewriting when variable is added later on
data EnemyType = LaserType | SeekerType | SwarmType | BugType
enemyType :: Enemy -> EnemyType
enemyType (Laser {}) = LaserType
{-enemyType (Seeker {}) = SeekerType
enemyType (Swarm  {}) = SwarmType
enemyType (Bug    {}) = BugType -}

newEnemy :: Float -> Float -> Float -> Float -> Enemy
newEnemy posY speedX amplY sinTime = (Laser ((fst halfScreenSize + 100 ,posY),(-speedX,0)) (sinTime, amplY) (1.5,1.5) [RectangleF (-50,-40) (50,40), RectangleF (-60,-32) (60,32)] (Fin 1) (Fin 1) (Index 0) 10)

updateMovementEn :: Float -> Enemy -> Enemy
updateMovementEn plust en = let (sint,a) = amplitudeMov en in
                            let (pos,(mvtx,mvty)) = placement en in
                            let sint' = sint + plust in
                            let mvty' = a * (cos (sint' * 50 / a)) in
                              en { amplitudeMov = (sint',a), placementEn = (pos,(mvtx,mvty'))}

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
  updateAnimation :: a -> a
class Viewables a where
  views :: Sprites -> a -> [Picture] -> [Picture]
  updateAnimations :: a -> a

class Positionable a => Collideable a where
  collision :: a -> Collision
class (Viewables a) => ViewCollideables a where
  viewAllCollision :: a -> [Picture] -> [Picture]

class (Collideable a) => Hurtable a where
  health   :: a -> Health
  getDamage :: Health -> a -> a

class (Collideable a) => Painful a where
  damage   :: a -> Health

class (Hurtable a) => Scoreable a where
  score    :: a -> ScorePoints

---------------------------------------------------------------------------------------
--Functions for class types

updateHealth1 :: (Hurtable a, Painful a, Scoreable b, Hurtable b, Painful b) => (ScorePoints,a,[b]) -> (ScorePoints,a,[b])
updateHealth1 (pt,obj1, [])           = (pt,obj1, [])
updateHealth1 (pt,obj1, (obj2:obj2s)) | toDespawnByHealth obj1 = (pt, obj1, (obj2:obj2s))
                                      | otherwise              = (pt''', obj1''', obj2s''') where
  hit              = objectsCollide obj1 obj2
  (pt''', obj1''', obj2s''') | hit       = (pt'', obj1'', obj2s'')
                             | otherwise = let (a1,a2,a3) = updateHealth1 (pt, obj1, obj2s) in (a1,a2,obj2:a3)
  obj1'            = getDamage (damage obj2) obj1
  obj2'            = getDamage (damage obj1) obj2
  (pt', obj1'', obj2s') = updateHealth1 (pt, obj1', obj2s)
  (pt'', obj2s'')   | toDespawnByHealth obj2' = (pt' + score obj2', obj2s')
                    | otherwise               = (pt', obj2' : obj2s')

updateHealth2 :: (Scoreable a, Hurtable a, Painful a, Scoreable b, Hurtable b, Painful b) => (ScorePoints,[a],[b]) -> (ScorePoints,[a],[b])
updateHealth2 (pt, [], obj2s)           = (pt, [], obj2s)
updateHealth2 (pt, obj1s, [])           = (pt, obj1s, [])
updateHealth2 (pt, (obj1:obj1s), obj2s) = (pt''', obj1s'',obj2s'') where
  (pt', obj1', obj2s')    = updateHealth1 (pt, obj1,obj2s)
  (pt'', obj1s', obj2s'') = updateHealth2 (pt', obj1s,obj2s')
  (pt''', obj1s'')        | toDespawnByHealth obj1' = (pt'' + score obj1', obj1s')
                          | otherwise               = (pt'', obj1' : obj1s')

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

doDamage :: (Hurtable a, Painful b) => b -> a -> a
doDamage att = getDamage (damage att)

toDespawnByHealth :: (Hurtable a) => a -> Bool
toDespawnByHealth obj = health obj <= (Fin 0)

despawnByPosition :: (Positionable a) => [a] -> [a]
despawnByPosition = foldr g [] where
  g obj acc | toNotDespawn obj = obj:acc
            | otherwise        = acc

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
nextPosition :: Float -> Placement -> Placement
nextPosition offset p@((posx, posy), mvt@(mvtx, mvty)) = ((nextX offset (posx,mvtx), nextY offset (posy,mvty)),mvt)

nextX :: Float -> Vector -> Float
nextX offset (posx, mvtx)
  | posx+mvtx <= -xBounds = -xBounds -- left x bounds
  | posx+mvtx >= xBounds  = xBounds  -- right x bounds
  | otherwise = posx+mvtx
    where
        xBounds = (fromIntegral (fst screenSize) :: Float) / 2 + offset

nextY :: Float -> Vector -> Float
nextY offset (posy, mvty)
  | posy+mvty <= -yBounds = -yBounds  -- lower y bound
  | posy+mvty >= yBounds  = yBounds   -- upper y bound
  | otherwise = posy+mvty
    where
      yBounds = (fromIntegral (snd screenSize) :: Float) / 2 + offset

-- Animation
nextAnimation :: Animation -> Animation
nextAnimation Invisible = Invisible
nextAnimation (Index inx) = Index (inx + 1)

--View
--translate, scale but with vectors
translateV  :: Vector -> Picture -> Picture
translateV  (fl1, fl2) = translate  fl1 fl2
scaleV      :: Vector -> Picture -> Picture
scaleV      (fl1, fl2) = scale      fl1 fl2