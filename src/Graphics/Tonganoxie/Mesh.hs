{-# LANGUAGE GADTs, KindSignatures, StandaloneDeriving #-}
module Graphics.Tonganoxie.Mesh where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
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

import Linear.Quaternion.Utils

import Graphics.Tonganoxie.Material 

-- A 'Mesh' is our key data-structure.
data Mesh = Mesh 
  { points       :: Vector (Point V3 Double)
  , normals      :: Vector (V3 Double)
  , uvs          :: Vector (V2 Double)
  , materials    :: Vector (Material ())
  , uv_materials :: Vector (Material UV)
  , faces        :: [Face]  --- Order is not important
  } deriving Show

data Face where
  Face :: [Vertex a] -> MT a -> Face

instance Show Face where
  -- We explicitly case here to avoid neededing to carry the dictionary
  -- in every Face constructor
  show (Face vs mt@(MTUV _))   = "Face " ++ show vs ++ " " ++ show mt
  show (Face vs mt@(MTNoUV _)) = "Face " ++ show vs ++ " " ++ show mt
  
showFace :: [Vertex a] -> MT a -> String
showFace vs (MTUV _) = "Face " ++ show (vs :: [Vertex UV])

newtype PT = PT Int     deriving Show
newtype NO = NO Int     deriving Show

data MT :: * -> * where 
  MTUV   :: Int -> MT UV
  MTNoUV :: Int -> MT ()
  
deriving instance Show (MT a)

data Vertex uv = Vertex !PT uv !NO deriving Show

showPT :: PT -> String
showPT (PT i) = show (i + 1)

showNO :: NO -> String
showNO (NO i) = show (i + 1)
{-
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


materialName :: Material uv -> String
materialName (Material) = "color"
-}
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

-- scale using the given 3D vector. The normals are preserved. 
-- scale by zero makes bad things happen to normals.
scale :: V3 Double -> Mesh -> Mesh
scale ss m = m
  { points  = fmap (\ (A.P p) -> A.P $ liftU2 (*) ss p) (points m)
  , normals = fmap (normalize . liftU2 (*) ss') (normals m)
  }
  where ss' = fmap (1/) ss


instance Monoid Mesh where
    mempty = Mesh 
           { points    = V.empty 
           , normals   = V.empty
           , uvs       = V.empty
           , materials = V.empty
           , uv_materials = V.empty
           , faces     = []
           }

    mappend m1 m2 = Mesh 
                  { points       = points  m1 V.++ points m2
                  , normals      = normals m1 V.++ normals m2
                  , uvs          = uvs     m1 V.++ uvs m2
                  , materials    = V.empty
                  , uv_materials = V.empty
                  , faces        = faces m1     ++ fmap f (faces m2)
                  }
       where
           pOff     = V.length (points m1)
           nOff     = V.length (normals m1)
           uvOff    = V.length (uvs m1)
           mOff     = V.length (materials m1)
           uv_mOff  = V.length (uv_materials m1)
                          
           f :: Face -> Face
           f (Face vs m) = Face (fmap (g m) vs) (incM m)
           g :: MT a -> Vertex a -> Vertex a
           g m (Vertex (PT a) c (NO b)) 
              = Vertex (PT (a + pOff)) (incUV m c) (NO (b + nOff)) 

           incUV :: MT uv ->  uv -> uv
           incUV (MTNoUV _) ()     = ()
           incUV (MTUV   _) (UV i) = UV (i + uvOff)

           incM :: MT uv -> MT uv
           incM (MTNoUV i) = MTNoUV (i + mOff)
           incM (MTUV   i) = MTUV   (i + uv_mOff)

--------------------------------------------------------------------------------
-- Shapes

-- needs to be at least 1x1
plane :: V2 Int -> Material a -> Mesh
plane (V2 1 1) m = mesh
  where    
    mesh = Mesh
      { points  = fromList [ A.P (V3 x y 0) | V2 x y <- fmap (fmap (\ n -> n * 2 - 1)) uvs ]
      , normals = fromList [ V3 0 0 1 ]
      , uvs     = case m of
                    Material _ _ MatchUV -> fromList $ uvs
                    _ -> V.empty
      , materials    = fromList $ [ mt | Just mt <- [addNoUVMaterial m] ]
      , uv_materials = fromList $ [ mt | Just mt <- [addUVMaterial   m] ]
      , faces   = [ Face [ Vertex (PT i)  (materialUV m i) (NO 0)| i <- [0..3]] 
                  $ mkMT m
                  ]
      }

    materialUV :: Material a -> Int -> a
    materialUV (Material _ _ MatchUV) i   = UV i
    materialUV (Material _ _ MatchNoUV) i = ()
    
    uvs :: [V2 Double]
    uvs = [V2 0 0, V2 1 0, V2 1 1, V2 0 1]


mkMT :: Material a -> MT a
mkMT (Material _ _ MatchUV)   = MTUV 0
mkMT (Material _ _ MatchNoUV) = MTNoUV 0

{-

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
          [ "f " ++ unwords [ showPT a ++ "/" ++ showUV m c ++ "/" ++ showNO b
                            | Vertex a b c <- vs 
                            ]
          ]
        | Face vs m <- faces m 
        ] ++
        [ "# end of file" ]


-}

example = plane (V2 1 1) $ color (1,0,0)

