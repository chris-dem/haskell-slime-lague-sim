{-# LANGUAGE TemplateHaskell #-}

module App where

import Control.Monad.Random (MonadRandom (getRandom, getRandomR), MonadTrans (lift), Rand, StdGen, foldM, forM, forM_, mkStdGen, replicateM, runRand, runRandT, when)
import Control.Monad.ST (runST)
import Control.Monad.State (MonadState (get), StateT (StateT, runStateT), evalStateT, execStateT, gets)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Maybe (fromJust, isJust, mapMaybe)
import Data.Tuple (swap)
import Debug.Trace (traceShow)
import Data.Massiv.Array as A
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

gridSize :: (Int, Int)
gridSize = (48, 32)

--- W, H
gridFactor :: (Float, Float)
gridFactor = (800 / 48, 600 / 32)

lerp :: Float -> Float -> Float -> Float
lerp x y t = x * t + y * (1 - t)

getWinSize = bimap (fromIntegral w *) (fromIntegral h *) gridFactor
  where
    (w, h) = gridSize

window :: Display
window = InWindow "Nice window?" (bimap floor floor getWinSize) (100, 100)

bgColour :: Color
bgColour = black

drawing :: Picture
drawing = color white $ thickCircle 80 5

type ImageType = A.Array U Ix2 Float

data SimSet = SimSet
    { _movSpeed :: Float
    , _numSlimes :: Int
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

getIndxSlime :: Slime -> Int
getIndxSlime = getIndxPair . bimap floor floor . (^. pos)

initState :: StdGen -> SimSet -> AppState
initState gen set = uncurry ($) $ runRand (evalStateT mainInit emptySt) gen
  where
    emptySt = V.replicate (gridSize & uncurry (*)) 0
    mainInit :: AppM (StdGen -> AppState)
    mainInit = do
        s <- replicateM (set ^. numSlimes) randomSlime
        forM_ s $ \slime -> do
            let i = getIndxSlime slime
            ix i .= 1.0
        img <- get
        return (,SimState{_slimes = s, _settings = set, _image = img})

    randomSlime :: AppM Slime
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

drawSquare :: V.Vector Float -> (Int, Int) -> Picture
drawSquare im (x, y) =
    color col
        $ translate
            (-(wW / 2) + fromIntegral x * fst gridFactor)
            (-(wH / 2) + fromIntegral y * snd gridFactor)
        $ uncurry rectangleSolid gridFactor
  where
    (wW, wH) = getWinSize
    (w, h) = gridSize
    ind = getIndxPair (x, y)
    intensity = im V.! ind
    col = greyN intensity

render :: AppState -> Picture
render st = pictures [draw (x, y) | x <- [0 .. w - 1], y <- [0 .. h - 1]]
  where
    state = st ^. _2
    comp = V.any (/= 0) (state ^. image)
    gridSet = state ^. settings
    (w, h) = gridSize
    im = state ^. image
    draw = drawSquare im

addP :: (Int, Int) -> (Int, Int) -> Maybe Int
addP (x, y) (dx, dy) =
    if w > x' && x' >= 0 && h > y' && y' >= 0
        then
            Just $ getIndxPair (x', y')
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
        let newImage = runST $ do
                mV <- V.unsafeThaw im
                applyDefusion mV dt
                drawSlimes newSlimes mV
                V.freeze mV
        slimes .= newSlimes
        image .= newImage
        return ()

    set = st ^. _2 . settings

    updateSlimePosition :: Float -> ImageType -> Slime -> Rand StdGen Slime
    updateSlimePosition dt im s = do
        r <- getRandomR (-1.0, 1.0)
        let angle = s ^. sAngle + r * (set ^. angleDt)
        let (w, h) = bimap fromIntegral fromIntegral gridSize
        let clamp m = min (m - 1) . max 0
        let newPos = s ^. pos VA.+ (mulSV (dt * 15.0) $ unitVectorAtAngle angle)
        let nPos = newPos & bimap (clamp w) (clamp h)
        return Slime{_sAngle = angle, _pos = nPos}

    drawSlimes :: [Slime] -> MV.MVector s Float -> ST s ()
    drawSlimes sm img = forM_ sm $ \s -> do
        let indx = getIndxSlime s
        MV.write img indx 1

    applyDefusion :: MV.MVector s Float -> Float -> ST s ()
    applyDefusion im dt = forM_ [(x, y) | x <- [0 .. w - 1], y <- [0 .. h - 1]] $ \p -> do
        let xt = mapMaybe (addP p) [(dx, dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1]]
        nVals <- forM xt $ MV.read im
        curr <- MV.read im (getIndxPair p)
        let blurred = sum nVals / 9
        let diffused = lerp curr blurred (0.1 * dt)
        let evap = max 0 (diffused - 0.01 * dt)
        MV.write im (getIndxPair p) evap
      where
        (w, h) = gridSize

mainApp :: IO ()
mainApp = do
    gen <- getStdGen
    simulate
        window
        black
        60
        (initState gen setting)
        render
        (const step)
  where
    setting =
        SimSet
            { _movSpeed = 10
            , _numSlimes = 10
            , _angleDt = pi / 4
            }
