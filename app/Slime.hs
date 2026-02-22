{-# LANGUAGE TemplateHaskell #-}

module Slime where

import Graphics.Gloss (Vector)
import Graphics.Gloss.Data.Point.Arithmetic qualified as VA
import Graphics.Gloss.Data.Vector (mulSV, unitVectorAtAngle)
import Lens.Micro.Platform

data Slime = Slime
    { _pos :: Vector
    , _sAngle :: Float
    }

makeLenses 'Slime

updateSlime :: Float -> Float -> Slime -> Slime
updateSlime speed dt s = s & pos %~ (VA.+ (vc `mulSV` dir))
  where
    dir = s ^. sAngle & unitVectorAtAngle
    vc = dt * speed
