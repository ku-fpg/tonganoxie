{-# LANGUAGE GADTs, KindSignatures #-}
module Graphics.Tonganoxie.Mesh where

import Data.Text (Text)
import Data.Vector (Vector, toList, fromList)

import Linear.Affine (Point) 
import qualified Linear.Affine as Affine
import Linear.V3 (V3(V3))
import Linear.V2 (V2(V2))

data Mesh = Mesh 
  { points  :: Vector (Point V3 Double)
  , normals :: Vector (V3 Double)
  , uvs     :: Vector (V2 Double)
  , faces   :: [Face]  --- Order is not important
  }

data Face where
  Face :: [Vertex a] -> Material a -> Face

newtype PT = PT Int
newtype NO = NO Int
newtype UV = UV Int

data NoUV = NoUV

data Vertex uv = Vertex !PT !NO uv

--------------------------------------------------------------------------------

data Material :: * -> * where
  Material ::         Material NoUV
  Texture  :: Text -> Material UV

materialUV :: Material uv -> Int -> uv
materialUV (Material) _ = NoUV
materialUV (Texture {}) i = UV i

--------------------------------------------------------------------------------
-- Shapes

-- needs to be at least 1x1
plane :: V2 Int -> Material a -> Mesh
plane (V2 1 1) m = mesh
  where    
    mesh = Mesh
      { points  = fromList [ Affine.P (V3 x y 0) | V2 x y <- uvs ]
      , normals = fromList [ V3 0 0 1 ]
      , uvs     = fromList $ uvs
      , faces   = [Face [ Vertex (PT i) (NO 0) (materialUV m i) | i <- [0..3]] m ]
      }
    uvs :: [V2 Double]
    uvs = [V2 0 0, V2 0 1, V2 1 1, V2 1 0]





{-  
instance Monoid Mesh where



  remap :: Int -> a -> a
  
      
rotate :: Quaternion Double -> Mesh -> Mesh

scale :: V3 Double -> Mesh -> Mesh

translate :: V3 Double -> Mesh -> Mesh

group :: Text -> Mesh -> Mesh

-- needs to be at least 1x1
plane :: V2 Int -> Material a -> Mesh
plane (V2 1 1) m = Mesh
  { points  = V.fromList [ Point (V3 1 1 0) ]
  , normals = V.fromList [ V3 0 0 1 ]
  , uvs     = V.fromList [ ... ]
  , faces   = [ Face [ Vertex p (N 0) (materialUV m uv) | (p,uv) <- [0..3]] m ]
  }

color :: Text -> Material NoUV

materialUV :: Material uv -> Int -> uv
-}

--------------------------------------------------------------------------------

instance Show Mesh where
    show m = unlines $
        [ "# generated with obj-tools" ] ++
        [ "v" ++ concatMap (\ v -> " " ++ show v) [x,y,z] 
        | (Affine.P (V3 x y z)) <- toList $ points m 
        ] ++
        [ "vt" ++  concatMap (\ v -> " " ++ show v) [u,v] 
        | (V2 u v) <- toList $ uvs m 
        ] ++
        [ "vn" ++  concatMap (\ v -> " " ++ show v) [x,y,z] 
        | (V3 x y z) <- toList $ normals m 
        ] ++
{-
        [ unlines $
          ["usemtl ..."] ++
          [ "f" ++ concatMap (\ v -> " " ++ show v) [x,y,z]  
          | v <- vs
          ]
        | (Face vs m) <- toList $ normals m 
        ] ++
-}
        [ "# end of file" ]
        