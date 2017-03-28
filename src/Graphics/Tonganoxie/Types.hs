{-# OPTIONS_GHC -Wall #-}
module Graphics.Tonganoxie.Types where

import Linear.Affine (Point)
import Linear.V2 (V2)
import Linear.V3 (V3(V3))
import Linear.V4 (V4(V4))


-- | 'UV' is used to index into a vector of UV values.
newtype UV = UV Int     deriving (Show,Eq,Ord)

-- | 'PT' is used to index into a vector of points.
newtype PT = PT Int     deriving (Show,Eq,Ord)

-- | 'NO' is used to index into a vector of normals.
newtype NO = NO Int     deriving (Show,Eq,Ord)  


-- | Points in 2D space
type R2 = Point V2 Double

-- | Points in 3D space
type R3 = Point V3 Double


-- | A 'Surface' is map from 2D/UV points, to 3D points.
type Surface = R2 -> R3

-- | A 'Polygon' is a shape on a plane, with a single normal.
-- This can be determined by looking at the first three points.
-- There will always be at least 3 points.
class Functor p => Polygon p where
  vertices :: p a -> [a]

instance Polygon V3 where
  vertices (V3 a b c) = [a,b,c]

instance Polygon V4 where
  vertices (V4 a b c d) = [a,b,c,d]
