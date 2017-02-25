{-# LANGUAGE GADTs, KindSignatures #-}
module Graphics.Tonganoxie.Mesh where

import Data.Text (Text)
import Data.Vector (Vector, toList, fromList)
import qualified Data.Vector as V

import Linear.Affine (Point, (.+^)) 
import qualified Linear.Affine as A
import Linear.Quaternion (Quaternion)
import qualified Linear.Quaternion as Q
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

showPT :: PT -> String
showPT (PT i) = show (i + 1)

showNO :: NO -> String
showNO (NO i) = show (i + 1)

--------------------------------------------------------------------------------

data Material :: * -> * where
  Material ::         Material NoUV
  Texture  :: Text -> Material UV

materialUV :: Material uv -> Int -> uv
materialUV (Material) _   = NoUV
materialUV (Texture {}) i = UV i

showUV :: Material uv -> uv -> String
showUV (Material)   NoUV   = ""
showUV (Texture {}) (UV i) = show (i + 1)

incUV :: Material uv -> Int -> uv -> uv
incUV (Material)   _ NoUV   = NoUV
incUV (Texture {}) n (UV i) = UV (n + i)

materialName :: Material uv -> String
materialName (Material) = "color"

--------------------------------------------------------------------------------
-- Operations

-- rotate using the given 'Quaternion'. The normals are preserved,
-- because they admit the same rotation.
rotate :: Quaternion Double -> Mesh -> Mesh
rotate r m = m
  { points  = fmap (\ (A.P p) -> A.P $ Q.rotate r $ p) (points m)
  , normals = fmap (Q.rotate r) (normals m)
  }
  
-- translate using the given 3D vector. The normals are preserved.
translate :: V3 Double -> Mesh -> Mesh
translate dd m = m
  { points  = fmap (.+^ dd) (points m)
  }

instance Monoid Mesh where
    mempty = Mesh 
           { points  = V.empty 
           , normals = V.empty
           , uvs     = V.empty
           , faces   = []
           }
    mappend m1 m2 = Mesh 
                  { points  = points  m1 V.++ points m2
                  , normals = normals m1 V.++ normals m2
                  , uvs     = uvs     m1 V.++ uvs m2
                  , faces   = faces m1     ++ fmap f (faces m2)
                  }
       where
           pOff  = V.length (points m1)
           nOff  = V.length (normals m1)
           uvOff = V.length (uvs m1)
           f :: Face -> Face
           f (Face vs m) = Face (fmap (g m) vs) m
           g :: Material a -> Vertex a -> Vertex a
           g m (Vertex (PT a) (NO b) c) 
              = Vertex (PT (a + pOff)) (NO (b + nOff)) (incUV m uvOff c)

--------------------------------------------------------------------------------
-- Shapes

-- needs to be at least 1x1
plane :: V2 Int -> Material a -> Mesh
plane (V2 1 1) m = mesh
  where    
    mesh = Mesh
      { points  = fromList [ A.P (V3 x y 0) | V2 x y <- uvs ]
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

-}

--------------------------------------------------------------------------------

-- We use the .obj format to show the Mesh
instance Show Mesh where
    show m = unlines $
        [ "# generated with obj-tools" ] ++
        [ "v" ++ concatMap (\ v -> " " ++ show v) [x,y,z] 
        | (A.P (V3 x y z)) <- toList $ points m 
        ] ++
        [ "vt" ++  concatMap (\ v -> " " ++ show v) [u,v] 
        | (V2 u v) <- toList $ uvs m 
        ] ++
        [ "vn" ++  concatMap (\ v -> " " ++ show v) [x,y,z] 
        | (V3 x y z) <- toList $ normals m 
        ] ++
        [ unlines $
          ["# usemtl ..."] ++
          [ "f " ++ unwords [ showPT a ++ "/" ++ showNO b ++ "/" ++ showUV m c 
                            | Vertex a b c <- vs 
                            ]
          ]
        | Face vs m <- faces m 
        ] ++
        [ "# end of file" ]


example = plane (V2 1 1) Material
