{-# LANGUAGE TemplateHaskell #-}

module App where

import Data.Massiv.Array.Stencil as S
import Control.Monad.Random (MonadRandom (getRandom, getRandomR), MonadTrans (lift), Rand, StdGen, foldM, forM, forM_, mkStdGen, replicateM, runRand, runRandT, when)
import Data.Word (Word8)
import Control.Monad.ST (runST)
import Control.Monad.State (MonadState (get), StateT (StateT, runStateT), evalStateT, execStateT, gets)
import qualified Data.ByteString.Internal as BS
import Data.Bifunctor (Bifunctor (bimap))
import qualified Data.Vector.Storable as VS
import Data.Maybe (fromJust, isJust, mapMaybe)
import Data.Tuple (swap)
import Debug.Trace (traceShow)
import  Data.Massiv.Array.Stencil as S
import qualified Data.Massiv.Array as A
import qualified Data.Massiv.Array.Mutable as MA
import GHC.Float qualified as Math
import GHC.ST (ST (ST))
import Graphics.Gloss
import Graphics.Gloss.Data.Picture (blank)
import Graphics.Gloss.Data.Point.Arithmetic qualified as VA
import Graphics.Gloss.Data.Vector (mulSV, unitVectorAtAngle)
import Graphics.Gloss.Data.ViewPort (ViewPort)
import Graphics.Gloss.Interface.IO.Simulate (simulateIO)
import Lens.Micro.Platform
import Slime
import System.Random
import Data.Massiv.Array (avgStencil)
import Graphics.Gloss.Interface.IO.Game (Event(EventKey), Key (SpecialKey), SpecialKey (KeyEsc), KeyState (Down), playIO)
import System.Exit (exitSuccess)
import System.IO.Unsafe (unsafePerformIO)
import Foreign.Ptr (plusPtr)
import Foreign.Storable (Storable(poke))

gridSize :: (Int, Int)
gridSize = (400, 300)


winDims :: (Float, Float)
winDims = (800, 600)

--- W, H
gridFactor :: (Float, Float)
gridFactor = winDims & bimap  ((/ fromIntegral w) ) ((/fromIntegral h))
    where
        (w,h) = gridSize



lerp :: Float -> Float -> Float -> Float
lerp x y t = x * t + y * (1 - t)


window :: Display
window = InWindow "Nice window?" (bimap floor floor winDims) (100, 100)

bgColour :: Color
bgColour = black

drawing :: Picture
drawing = color white $ thickCircle 80 5

type ImageType = A.Array A.P A.Ix2 Float
type MImageType s = MA.MArray s A.P A.Ix2 Float

data SimSet = SimSet
    { _movSpeed :: Float
    , _numSlimes :: Int
    , _diffSpeed :: Float
    , _evapSpeed :: Float
    , _senseDist :: Float
    , _senseTheta :: Float
    , _senseWin :: Int
    , _angleDt :: Float
    }

data SimState = SimState
    { _slimes :: [Slime]
    , _settings :: SimSet
    , _image :: ImageType
    }

type AppState = (StdGen, SimState)

makeLenses ''SimSet
makeLenses ''SimState

type AppM = StateT ImageType (Rand StdGen)

getIndxPair :: (Int, Int) -> Int
getIndxPair (x, y) = i
  where
    (w, _) = gridSize
    i = y * w + x


writeSlimesImg :: ImageType -> [Slime] -> ImageType 
writeSlimesImg im slimes = snd $ 
    runST $ A.withMArrayS im $ \arr -> 
                forM_ slimes $ \s -> do
                    let (x, y) = s ^. pos & bimap floor floor
                    A.writeM arr (y A.:. x) 1.0

getIndxSlime :: Slime -> Int
getIndxSlime = getIndxPair . bimap floor floor . (^. pos)

initState :: StdGen -> SimSet -> AppState
initState gen set = uncurry ($) $ runRand mainInit gen
  where
    (w, h) = gridSize
    mainInit :: RandomGen g => Rand g (StdGen -> AppState)
    mainInit = do
        slimes <-  replicateM (set ^. numSlimes) randomSlime
        let empt =  (A.replicate A.Par (A.Sz (h A.:. w))  0.0) :: ImageType
        return  (,SimState{_slimes =  slimes, _settings = set, _image = writeSlimesImg empt slimes})

    randomSlime :: RandomGen g => Rand g Slime
    randomSlime = do
        let (width, height) = gridSize
        x <- getRandomR (0, fromIntegral width - 1)
        y <- getRandomR (0, fromIntegral height - 1)
        theta <- getRandomR (0, 2 * Math.pi)
        return $
            Slime
                { _pos = (x, y)
                , _sAngle = theta
                }

drawSquare :: ImageType -> A.Ix2 -> Picture
drawSquare im ix@(y A.:. x) =
    if intensity == 0 then blank else 
        color col $ translate
            (-(wW / 2) + fromIntegral x * fst gridFactor)
            (-(wH / 2) + fromIntegral y * snd gridFactor)
        $ uncurry rectangleSolid gridFactor
  where
    (wW, wH) = winDims
    (w, h) = gridSize
    intensity =   im A.! ix
    col = greyN intensity

render :: AppState -> Picture
render st = scale sW sH $ renderToBitMap (st ^. _2 .image)
  where
    (wW, wH) = winDims
    (sW, sH) = gridFactor


renderToBitMap :: ImageType -> Picture
renderToBitMap arr = bitmapOfByteString w h bpFormat im False
  where
    (w, h) = gridSize
    bpFormat = BitmapFormat { rowOrder = BottomToTop, pixelFormat = PxRGBA }

    vec = A.toStorableVector $ A.computeAs A.S arr

    len = VS.length vec

    clampToWord8 :: Float -> Word8
    clampToWord8 x = fromIntegral (floor (max 0 (min 1 x) * 255) :: Int)

    im = BS.unsafeCreate (4 * len) $ \ptr -> do
      let go i
            | i == len = return ()
            | otherwise = do
                let x = VS.unsafeIndex vec i
                    val = clampToWord8 x
                    base = plusPtr ptr (4 * i)
                poke base (0 :: Word8)
                poke (plusPtr base 1) val
                poke (plusPtr base 2) (0 :: Word8)
                poke (plusPtr base 3) (255 :: Word8)
                go (i + 1)
      go 0

addP :: (Int, Int) -> (Int, Int) -> Maybe A.Ix2
addP (x, y) (dx, dy) =
    if w > x' && x' >= 0 && h > y' && y' >= 0
        then
            Just $ (y' A.:. x')
        else
            Nothing
  where
    (x', y') = (x + dx, y + dy)
    (w, h) = gridSize


clamp :: Int -> Float -> Float
clamp m  x = max 0 $ min (fromIntegral m -1) x

step :: Float -> AppState -> IO AppState
step dt st = pure $ swap $ runRand (execStateT mainStep (st ^. _2)) (st ^. _1)
  where
    mainStep :: StateT SimState (Rand StdGen) ()
    mainStep = do
        s <- use slimes
        im <- use image
        newSlimes <- lift $ mapM (updateSlimePosition dt im) s
        slimes .= newSlimes
        image %= drawSlimes newSlimes . flip applyDefusion dt
        return ()

    set = st ^. _2 . settings


    updateSlimePosition :: Float -> ImageType -> Slime -> Rand StdGen Slime
    updateSlimePosition dt im s = do
        randSteerStrength <- getRandomR (0.0, 1.0)
        let weightStraight = sense 0
        let weightLeft = sense (set ^. senseTheta)
        let weightRight = sense (- set ^. senseTheta)
        let turnSpeed = set ^. angleDt * 2 * pi
        let angleC = if (weightStraight > max weightLeft weightRight) then 0 else
                if (weightStraight < min weightLeft weightRight) then (randSteerStrength - 0.5)  * 2 * turnSpeed* dt else
                    if (weightRight > weightLeft) then - randSteerStrength   * turnSpeed* dt else
                        randSteerStrength   * turnSpeed* dt 
        let newAngle = s ^. sAngle + angleC 
        let dir = unitVectorAtAngle newAngle
        let (w, h) =  gridSize

        let newPos = s ^. pos VA.+ (mulSV (dt * set ^. movSpeed) dir)
        let nAng = if newPos ^. _1 < 0 ||  newPos ^.  (_1 . to ceiling)   >= w || newPos ^. _2 < 0 || newPos ^. (_2 . to ceiling) >= h then newAngle - pi else newAngle
        let nPos =  newPos & bimap (clamp w) (clamp h)
        return Slime{_sAngle = nAng, _pos = nPos}
        where 
            sense :: Float ->  Float
            sense angle = v
                where
                    ang = s ^. sAngle + angle
                    dir = unitVectorAtAngle ang
                    (x,y) = s ^.pos VA.+ (mulSV (set ^. senseDist) dir) & bimap floor floor
                    foo (Just x ) = x
                    foo Nothing = 0
                    v = sum [foo $ A.index im  (y A.:. x) |dx <- [- set ^.senseWin ..  set ^.senseWin], dy <- [- set ^.senseWin ..  set ^.senseWin]]



    drawSlimes :: [Slime] -> ImageType -> ImageType
    drawSlimes sm img = snd $ runST $ A.withMArrayS img $ \marr -> do
        forM_ sm $ \slime -> do
            let (x,y) = slime ^. pos & bimap floor floor
            A.writeM marr (y A.:. x) 1.0

    applyDefusion :: ImageType -> Float -> ImageType
    applyDefusion im dt = A.computeP final
      where
        t = max 0 $ min 1 $ dt * (set ^. diffSpeed)
        evap = set ^. evapSpeed * dt   -- precompute small constant
        -- -- 3×3 average stencil with constant 0 padding at borders
        avg3x3Stencil = A.makeStencil (A.Sz (3 A.:. 3)) (1 A.:. 1) $ \get ->
            let 
                center = get (0 A.:.0)
                sm = (get (0  A.:. -1)
                    + get (0  A.:.  1)
                    + get (-1 A.:. -1)
                    + get (-1 A.:.  0) 
                    + get (-1 A.:.  1)
                    + get (1  A.:. -1)
                    + get (1  A.:.  0)
                    + get (1  A.:.  1))
                blurred = (center + sm)/9
                diffused = t * center + (1 -t)* blurred - evap
            in 
                max 0 $  diffused

        -- Apply stencil to im, producing blurred version (delayed)
        -- final = A.applyStencil (A.samePadding (avgStencil 3) (A.Fill 0.0)) (avgStencil 3) im
        final = A.applyStencil (A.samePadding (avg3x3Stencil) (A.Fill 0.0)) (avg3x3Stencil) im

handleEvent :: Event -> AppState -> IO AppState
handleEvent (EventKey (SpecialKey KeyEsc) Down _ _) _ = exitSuccess
handleEvent _ st = pure st

mainApp :: IO ()
mainApp = do
    gen <- getStdGen
    playIO
        window
        black
        120
        (initState gen setting)
        (pure. render)
        handleEvent
        (const step 0.01)
  where
    setting =
        SimSet
            { _movSpeed = 50
            , _numSlimes = 150
            , _angleDt = 2
            , _diffSpeed = 0.1
            , _senseDist = 5
            , _senseTheta = pi/ 4
            , _evapSpeed = 0.1
           , _senseWin  = 2
            }
