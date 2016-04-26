module Main where

import Data.Word ( Word8 )
import Control.Applicative ( (<$>) )
import Control.Monad ( forM )
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as SDLi
import qualified Graphics.UI.SDL.TTF.General as TTFG
import qualified Graphics.UI.SDL.TTF.Management as TTFM
import qualified Graphics.UI.SDL.TTF.Render as TTFR
import qualified Graphics.UI.SDL.TTF.Types as TTFT
import System.CPUTime ( getCPUTime )
--import Control.Concurrent.Thread.Delay (delay)
import Data.Atom.Simple ( Symbol, intern )

import Levels

loadImage :: String -> Maybe (Word8, Word8, Word8) -> IO SDL.Surface
loadImage filename colorKey = SDLi.load filename >>= SDL.displayFormat >>= setColorKey colorKey

setColorKey :: Maybe (Word8, Word8, Word8) -> SDL.Surface -> IO SDL.Surface
setColorKey Nothing s = return s
setColorKey (Just (r, g, b)) surface = (SDL.mapRGB . SDL.surfaceGetPixelFormat) surface r g b >>= SDL.setColorKey surface [SDL.SrcColorKey] >> return surface

applySurface :: Int -> Int -> SDL.Surface -> SDL.Surface -> Maybe SDL.Rect -> IO Bool
applySurface x y src dst clip = SDL.blitSurface src clip dst (Just $ SDL.Rect x y 0 0)

getCPUTimeDouble :: IO Double
getCPUTimeDouble = (\t -> fromInteger t * 1e-12) <$> getCPUTime

type DTime = Double

type Hash a = [(Symbol, a)]

data Screen = Screen { screenSurface :: SDL.Surface
                     , screenWidth   :: Int
                     , screenHeight  :: Int
                     , screenBpp     :: Int
                     }

data App = App { appScreen  :: Screen
               , appPlayer  :: Beast
               , appHandle  :: Handle
               , appFont    :: TTFT.Font
               , appLevels  :: [Level]
               , appDtime   :: DTime
               --, counter :: Int
               --, second  :: DTime
               , appImages  :: Hash SDL.Surface
               }

data Handle = Handle { handleQuit :: Bool
                     , handleKeys :: [SDL.SDLKey]
                     }

plus :: (Num a) => (a, a) -> (a, a) -> (a, a)
plus (x, y) (z, t) = (x + z, y + t)

digits :: Int -> [Int]
digits num = if num > 10 then digits (div num 10) ++ [mod num 10] else [num]

sqr :: (Num a) => a -> a
sqr x = x * x

checkCollision :: Beast -> Zone -> Bool
checkCollision pl (ZoneAllow (u1, u2) (u3, u4)) = (u1 <= v1) && (u2 <= v2) && (v3 <= u3+1) && (v4 <= u4+1)
  where
    (v1, v2) = beastPosition pl
    (v3, v4) = (v1 + 16, v2 + 16)
checkCollision pl (ZoneForbid (u1, u2) (u3, u4)) = (v3 <= u1+1) || (v4 <= u2+1) || (v1 >= u3) || (v2 >= u4)
  where
    (v1, v2) = beastPosition pl
    (v3, v4) = (v1 + 16, v2 + 16)
checkCollision _ (ZoneWin _ _) = True

hashFind :: String -> Hash a -> a
hashFind key hash = (snd.head) $ filter ((==intern key).fst) hash

initialize :: IO App
initialize = do
  SDL.init [SDL.InitEverything]
  TTFG.init
  screen <- SDL.setVideoMode width height bpp [SDL.SWSurface]
  SDL.setCaption "Hardest game" []
  mainFont <- TTFM.openFont "lazy.ttf" 28
  cTime    <- getCPUTimeDouble
  imags <- forM imgs (\(file, params) -> loadImage (file ++ ".png") params)
  let hash = zipWith (\(file, _) im -> (intern file, im)) imgs imags in
    return $ App (Screen screen width height bpp) (Player (80, 240) 0 150) (Handle False []) mainFont startLevels cTime hash
    where
      width = 640
      height = 480
      bpp = 32
      imgs = [("player", Nothing), ("enemy", Just (255, 255, 255)), ("gold", Just (255, 255, 255))]

cleanUp :: App -> IO ()
cleanUp app = do
  mapM_ (SDL.freeSurface.snd) $ appImages app
  TTFG.quit
  SDL.quit

delFromList :: (Eq a) => a -> [a] -> [a]
delFromList _ [] = []
delFromList a (x:xs) = if a == x then xs else x : delFromList a xs

events :: Handle -> IO Handle
events h@(Handle q key) = do
  event <- SDL.pollEvent
  case event of
    SDL.Quit           -> return $ Handle True []
    SDL.KeyDown keysum -> events $ Handle q (SDL.symKey keysum : key)
    SDL.KeyUp   keysum -> events $ Handle q (delFromList (SDL.symKey keysum) key)
    SDL.NoEvent        -> return $ Handle False key
    _                  -> events h

lvlsEnemies :: [Level] -> [Beast] -> [Level]
lvlsEnemies [] _ = error "lvlsEnemies got empty levels list!"
lvlsEnemies (lvl:lvls) beast = (lvl {levelEnemies = beast}) : lvls

lvlsUpdate :: [Level] -> [Beast] -> [Gold] -> [Level]
lvlsUpdate [] _ _ = error "lvlsEnemies got empty levels list!"
lvlsUpdate (lvl:lvls) beasts golds = (lvl {levelEnemies = beasts, levelGolds = golds}) : lvls

prepareLoop :: App -> IO (Double, Handle)
prepareLoop app = do
  appRender app 
  cTime <- getCPUTimeDouble
  handle <- events $ appHandle app
  return (cTime, handle)

loop :: App -> IO App
loop app@(App _ player _ _ (lvl@(Saver _ time _):lvls) dtim _) = do
  (cTime, handle) <- prepareLoop app
  let timeForSaver = time - cTime + dtim
    in
      if timeForSaver > 0
        then let
         nApp = app {appLevels = (lvl {levelLength = timeForSaver}):lvls, appHandle = handle}
            in
              if handleQuit handle then return nApp else loop nApp
        else let
          nPlayer = player {beastPosition = levelStartPos $ head lvls}
          nApp = app {appLevels = lvls, appPlayer = nPlayer, appHandle = handle}
            in
              if handleQuit handle then return nApp else loop nApp

loop app@(App scr playerU _ fon levels@(Level startpos enemas zonas golds:leveles) dtim images) = do
  (cTime, handle) <- prepareLoop app
  let deltaTime = cTime - dtim
      nEmems = map (`beastTurn` deltaTime) enemas
      np = playerTurn playerU (handleKeys handle) zonas deltaTime
      (nPlayer, pickedGold) = if any (`collision` np) nEmems
                              then (playerDie np startpos, resetGold golds)
                              else (np, takeGold np golds)
    in
      if all goldTaked pickedGold && any (checkWin nPlayer) zonas
        then let
          mPlayer = nPlayer {beastPosition = levelStartPos $ head leveles}
          nApp = App scr mPlayer handle fon leveles cTime images
          in
            if handleQuit handle then return nApp else loop nApp
        else let
          nApp = App scr nPlayer handle fon (lvlsUpdate levels nEmems pickedGold) cTime images
          in
            if handleQuit handle then return nApp else loop nApp
--delay (10)
--where
--(sec, fpsCounter) = let con = 1 + counter app in if con >= 60 then (0,  else (con, second app)

checkWin :: Beast -> Zone -> Bool
checkWin pl (ZoneWin (u1, u2) (u3, u4)) = (u1 <= v1) && (u2 <= v2) && (v3 <= u3+1) && (v4 <= u4+1)
  where
    (v1, v2) = beastPosition pl
    (v3, v4) = (v1 + 16, v2 + 16)
checkWin _ _ = False

resetGold :: [Gold] -> [Gold]
resetGold = map (\g -> g {goldTaked = False})

playerDie :: Beast -> Pos -> Beast
playerDie player startPos = player {beastPosition = startPos, beastDeathCount = beastDeathCount player + 1}

playerTurn :: Beast -> [SDL.SDLKey] -> [Zone] -> DTime -> Beast
playerTurn pl [] _ _ = pl
playerTurn pl@(Player curtPos _ vC) (k:keys) zon dTime |SDL.SDLK_UP    == k = playerTurn (newPl (0, -vc)) keys zon dTime
                                                       |SDL.SDLK_DOWN  == k = playerTurn (newPl (0,  vc)) keys zon dTime
                                                       |SDL.SDLK_LEFT  == k = playerTurn (newPl (-vc, 0)) keys zon dTime
                                                       |SDL.SDLK_RIGHT == k = playerTurn (newPl (vc,  0)) keys zon dTime
                                                       |otherwise = playerTurn pl keys zon dTime
    where
      newPl (dx, dy) = let player = pl {beastPosition = plus curtPos (dx, dy)} in if all (checkCollision player) zon then player else pl
      vc = vC * dTime

collision :: Beast -> Beast -> Bool
collision (Enemy (x, y) _) (Player (px, py) _ _) = sqr (px - x) + sqr (py - y) <= 225

takeGold :: Beast -> [Gold] -> [Gold]
takeGold (Player (px, py) _ _) = map (\g@(Gold (x, y) tak) -> if not tak && (sqr (px - x) + sqr (py - y) <= 225) then g {goldTaked = True} else g)

countFreq :: Eq a => [a] -> a -> Int
countFreq [] _ = 0
countFreq (x:[]) a = if a == x then 1 else 0
countFreq (x:xs) a = if a == x then 1 + countFreq xs a else countFreq xs a

beastTurn :: Beast -> DTime -> Beast
beastTurn (Enemy (x, y) mv) delta = Enemy nPos nChar
  where
    (nPos, nChar) = enemyPos (x, y) mv delta

enemyPos :: Pos -> Character -> DTime -> (Pos, Character)
enemyPos (x, y) Static _ = ((x, y), Static)
enemyPos _ (Circle r speed angle (cx, cy)) delta = ((cx + r * sin nAngle, cy + r * cos nAngle), Circle r speed nAngle (cx, cy))
  where nAngle = angle + speed * delta
enemyPos (x, y) (Perpet node cur vel) delta
  | gipot < 1 = let nextNode = if limit == cur then 0 else cur + 1 in (target, Perpet node nextNode vel)
  | otherwise = let dy = ty - y
                    dx = tx - x in ((x + timedVel * dx / gipot, y + timedVel * dy / gipot), Perpet node cur vel)
  where
    limit = length node - 1
    target@(tx, ty) = node !! cur
    gipot = sqrt $ sqr (tx - x) + sqr (ty - y)
    timedVel = vel * delta

appRender :: App -> IO ()
appRender (App screen _ _ font (Saver text _ _:_) _ _) = do
  color <- (SDL.mapRGB . SDL.surfaceGetPixelFormat) surface 0x3f 0x3f 0x3f
  let clipRect = Just $ SDL.Rect 0 0 640 480
    in do
      SDL.fillRect surface clipRect color

      deathC <- TTFR.renderTextSolid font text (SDL.Color 0 0 0)
      applySurface 240 240 deathC surface Nothing
      SDL.flip surface
        where surface = screenSurface screen
appRender (App screen player _ font (level:_) _ images)  = do
  color <- (SDL.mapRGB . SDL.surfaceGetPixelFormat) surface 0x3f 0x3f 0x3f
  let clipRect = Just $ SDL.Rect 0 0 640 480
    in do
      SDL.fillRect surface clipRect color

      renderLevel level images surface
      renderBeast player (hashFind "player" images) surface
      deathC <- TTFR.renderTextSolid font (show $ beastDeathCount player) (SDL.Color 0 0 0)
      applySurface 600 0 deathC surface Nothing
      SDL.flip surface
        where surface = screenSurface screen

renderLevel :: Level -> Hash SDL.Surface -> SDL.Surface -> IO ()
renderLevel (Level _ enem zones gold) images screen = do
  mapM_ (`renderZone` screen) zones
  mapM_ (\x -> renderGold x (hashFind "gold" images) screen) gold
  mapM_ (\x -> renderBeast x (hashFind "enemy" images) screen) enem

renderGold :: Gold -> SDL.Surface -> SDL.Surface -> IO Bool
renderGold (Gold (dx, dy) False) pict screen = applySurface x y pict screen Nothing
  where
    x = floor dx
    y = floor dy
renderGold _ _ _ = return False -- if gold already taken - nothing to do here

renderBeast :: Beast -> SDL.Surface -> SDL.Surface -> IO Bool
renderBeast unit beastImage screen = applySurface x y beastImage screen Nothing
  where
    x = (floor . fst . beastPosition) unit
    y = (floor . snd . beastPosition) unit

renderZone :: Zone -> SDL.Surface -> IO Bool
renderZone (ZoneForbid (u1, u2) (u3, u4)) screen = renderZoneWithColor (u1, u2) (u3, u4) (0x3f, 0x3f, 0x3f) screen
renderZone (ZoneAllow  (u1, u2) (u3, u4)) screen = renderZoneWithColor (u1, u2) (u3, u4) (0xff, 0xff, 0xff) screen
renderZone (ZoneWin    (u1, u2) (u3, u4)) screen = renderZoneWithColor (u1, u2) (u3, u4) (0x88, 0xff, 0x88) screen

renderZoneWithColor :: (RealFrac a) => (a, a) -> (a, a) -> (Word8, Word8, Word8) -> SDL.Surface -> IO Bool
renderZoneWithColor (u1, u2) (u3, u4) (r, g, b) screen = do
  color    <- (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen r g b
  let clipRect = Just $ SDL.Rect (floor u1) (floor u2) (ceiling (u3 - u1)) (ceiling (u4 - u2)) in
    SDL.fillRect screen clipRect color

main :: IO ()
main = initialize >>= loop >>= cleanUp
