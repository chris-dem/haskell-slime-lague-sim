{-# LANGUAGE TemplateHaskell #-}

module App where

import Control.Monad.Random (MonadRandom (getRandom, getRandomR), MonadTrans (lift), Rand, StdGen, foldM, forM, forM_, mkStdGen, replicateM, runRand, runRandT, when)
import Control.Monad.ST (runST)
import Control.Monad.State (MonadState (get), StateT (StateT, runStateT), evalStateT, execStateT, gets)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Maybe (fromJust, isJust, mapMaybe)
import Data.Tuple (swap)
import Debug.Trace (traceShow)
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

gridSize :: (Int, Int)
gridSize = (100, 100)


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
render st = pictures [draw (y A.:.x) | x <- [0 .. w - 1], y <- [0 .. h - 1]]
  where
    state = st ^. _2
    gridSet = state ^. settings
    (w, h) = gridSize
    im = state ^. image
    draw = drawSquare im

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

step :: Float -> AppState -> AppState
step dt st = swap $ runRand (execStateT mainStep (st ^. _2)) (st ^. _1)
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
        r <- getRandomR (-1.0, 1.0)
        let angle = s ^. sAngle + r * (set ^. angleDt * dt)
        let (w, h) =  gridSize
        let clamp m = min (fromIntegral m - 1) . max 0
        let newPos = s ^. pos VA.+ (mulSV (dt * set ^. movSpeed) $ unitVectorAtAngle angle)
        let nPos = newPos & bimap (clamp w) (clamp h)
        return Slime{_sAngle = angle, _pos = nPos}

    drawSlimes :: [Slime] -> ImageType -> ImageType
    drawSlimes sm img = snd $ runST $ A.withMArrayS img $ \marr -> do
        forM_ sm $ \slime -> do
            let (x,y) = slime ^. pos & bimap floor floor
            A.writeM marr (y A.:. x) 1.0

    applyDefusion :: ImageType -> Float -> ImageType
    applyDefusion im dt = diffused
        where
            diff = (A.computeP $ A.applyStencil (A.samePadding (A.avgStencil 3) (A.Fill 0.0)) (A.avgStencil 3) im) :: ImageType
            t = max 0 $ min 1 $ dt * (set ^. diffSpeed)
            diffused = A.computeP $ A.map (max 0) $ (t A.*. im) A.!+! ((1 - t) A.*. diff) A..- (set ^. evapSpeed * dt)



mainApp :: IO ()
mainApp = do
    gen <- getStdGen
    simulate
        window
        black
        120
        (initState gen setting)
        render
        (const step)
  where
    setting =
        SimSet
            { _movSpeed = 50
            , _numSlimes = 25
            , _angleDt = 8 * pi
            , _diffSpeed = 0.1
            , _evapSpeed = 0.1
            }
