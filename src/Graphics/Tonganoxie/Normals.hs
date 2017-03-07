{-# LANGUAGE GADTs, KindSignatures, StandaloneDeriving, OverloadedStrings #-}
module Graphics.Tonganoxie.Normals where

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
import Linear.V3 (V3(V3), cross)
import Linear.V2 (V2(V2))
import Linear.Vector (liftU2,(^/),(*^))
import Linear.Metric(normalize, distance)

import System.FilePath (replaceExtension)

import Linear.Quaternion.Utils

import Graphics.Tonganoxie.Material 
import Graphics.Tonganoxie.Surface
import Graphics.Tonganoxie.Mesh



-- Given 
faceNormal :: Vector (Point V3 Double) -> V3 PT -> V3 Double
faceNormal pts idxs = normalize $ (p2 - p1) `cross` (p3 - p2)
  where V3 p1 p2 p3 = fmap f idxs
        f :: PT -> V3 Double
        f (PT i) = let (A.P p) = pts V.! i in p


surfaceNormal :: Int -> Surface -> Point V2 Double -> V3 Double
surfaceNormal dN f uv2@(A.P (V2 u v)) = id $ (p2 - p1) `cross` (p3 - p2)
  where
     uAxis = if u < 0.5 then V2 1 0 else V2 (-1) 0
     vAxis = if v < 0.5 then V2 0 1 else V2 0 (-1)

     dNf = fromIntegral dN

     uv1 = uv2 + A.P uAxis ^/ dNf
     uv3 = uv2 + A.P vAxis ^/ dNf
     A.P p1 = dNf *^ f uv1
     A.P p2 = dNf *^ f uv2
     A.P p3 = dNf *^ f uv3    
