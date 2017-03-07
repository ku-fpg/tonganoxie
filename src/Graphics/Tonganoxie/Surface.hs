{-# LANGUAGE GADTs, KindSignatures, StandaloneDeriving, OverloadedStrings #-}
module Graphics.Tonganoxie.Surface where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Vector (Vector, toList, fromList)
import qualified Data.Vector as V

import Linear.Affine (Point, (.+^)) 
import qualified Linear.Affine as A
import Linear.Quaternion (Quaternion)
import qualified Linear.Quaternion as Q
import Linear.V3 (V3(V3))
import Linear.V2 (V2(V2))
import Linear.Vector (liftU2)
import Linear.Metric(normalize, distance)

import System.FilePath (replaceExtension)

import Linear.Quaternion.Utils

import Graphics.Tonganoxie.Material 
import Graphics.Tonganoxie.Mesh

-- | A 'Surface' is map from UV points, to V3 points.
type Surface = Point V2 Double -> Point V3 Double

sphere :: Surface
sphere (A.P (V2 u v)) 
    = A.P $ V3 (sin long * cos lat)
               (sin lat)
               (cos long * cos lat)
  where 
     long = u * pi * 2     -- 0 .. 2pi
     lat  = (v - 0.5) * pi -- -pi/2 ... pi/2


plane :: Surface
plane (A.P (V2 u v)) = A.P $ V3 (f u) (f v) 0
  where
      f x = x * 2 - 1               
