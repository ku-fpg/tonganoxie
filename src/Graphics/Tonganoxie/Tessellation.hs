module Graphics.Tonganoxie.Tessellation where

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
import Linear.Vector (liftU2)
import Linear.Metric(normalize, distance)

import System.FilePath (replaceExtension)

import Linear.Quaternion.Utils

import Graphics.Tonganoxie.Material 
import Graphics.Tonganoxie.Mesh


data Tessellation p = Tessellation
 { points :: Vector p
 , faces  :: [V3 UV]
 } deriving Show
 
-- simple and direct grid of triangles.
tessellation :: V2 Int -> Tessellation (Point V2 Double)
tessellation (V2 x y) = Tessellation the_points the_faces
    where
        the_points = V.fromList 
            [ A.P $ V2 (fromIntegral u / fromIntegral x)
                       (fromIntegral v / fromIntegral y) 
            | u <- us
            , v <- vs
            ]
        the_faces = fmap (fmap ix)
             [ f 
             | (u,u') <- us `zip` tail us
             , (v,v') <- vs `zip` tail vs
             , f <- [ V3 (u,v) (u,v') (u',v')
                    , V3 (u',v') (u',v') (u,v)
                    ]
             ]
        ix (u,v) = UV $ u + v * (x + 1)
        us = [0..x] :: [Int]
        vs = [0..y] :: [Int]
        
