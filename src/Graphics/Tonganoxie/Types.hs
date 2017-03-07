module Graphics.Tonganoxie.Types where

import Linear.Affine (Point)
import Linear.V2 (V2)
import Linear.V3 (V3)


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




