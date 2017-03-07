{-# LANGUAGE GADTs, KindSignatures, StandaloneDeriving, OverloadedStrings #-}
module Graphics.Tonganoxie.Shapes where

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
import Graphics.Tonganoxie.Normals
import qualified Graphics.Tonganoxie.Surface as S
import Graphics.Tonganoxie.Surface (Surface)


import Graphics.Tonganoxie.Tessellation(Tessellation)
import qualified Graphics.Tonganoxie.Tessellation as T


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

example1 = plane (V2 1 1) $ color (1,0,0)

example2 = plane (V2 1 1) $ uvMaterial "dice"
  [ Kd 1 1 1
  , Map_Kd "dice.jpg"
  , Illum 0
  ]

  
{-
 * 2D-to-3D: V3 -> V2,
 * (uv)points : [V2]
 * faces: [[UV]]
 -}

data Shape uv = Shape 
  { shape_fn    :: uv -> V3 Double
  , shape_uv    :: [uv]
  , shape_faces :: [[UV]]
  }

planeShape :: Shape (V2 Double)
planeShape = Shape f uvs faces
 where
     f (V2 x y) = V3 (g x) (g y) 0
     g n = (n * 2) - 1
     uvs = [ V2 x y
           | x <- [0,1]
           , y <- [0,1]
           ]
     faces = map (map UV) 
           [ [1,0,2,3]
           ]


cubeShape :: Shape (Bool,Bool,Bool)
cubeShape = Shape f uvs faces
 where
     f (x,y,z) = g <$> V3 x y z
     g False = -1
     g True  = 1
     uvs = [ (x,y,z) 
           | x <- [False,True]
           , y <- [False,True]
           , z <- [False,True]
           ]
     faces = map (map UV) 
           [ [0,1,3,2]
           , reverse [4,5,7,6]
           , [0,4,5,1]
           , [1,5,7,3]
           , [3,7,6,2]
           , [2,6,4,0]
           ]
           
shape :: Shape a -> Material () -> Mesh
shape s m = Mesh
          { points  = the_points
          , normals = the_normals
          , uvs     = fromList $ []
          , materials    = fromList $ [ mt | Just mt <- [addNoUVMaterial m] ]
          , uv_materials = fromList $ [ ]
          , faces   = the_faces
          }
  where the_points = fromList $ map A.P $ map (shape_fn s) $ shape_uv $ s      
        the_faces  = [ Face [ Vertex (PT v) () (NO i) | (UV v) <- vs]
                     $ mkMT m
                     | (vs,i) <- shape_faces s `zip` [0..]
                     ]
        the_normals = fromList $ rawMeshFaceNormals $ RawMesh the_points 
                               $ [ [ pt | Vertex pt _ _ <- vs ] | Face vs _ <- the_faces ]
                              
uvShape :: Shape (V2 Double) -> Material a -> Mesh
uvShape s m = Mesh
          { points  = the_points
          , normals = the_normals
          , uvs     = case m of
                    Material _ _ MatchUV -> fromList $ uvs
                    _ -> V.empty
          , materials    = fromList $ [ mt | Just mt <- [addNoUVMaterial m] ]
          , uv_materials = fromList $ [ mt | Just mt <- [addUVMaterial   m] ]
          , faces   = the_faces
          }
  where the_points = fromList $ map A.P $ map (shape_fn s) $ shape_uv $ s      
        the_faces  = [ Face [ Vertex (PT v) (materialUV m v) (NO i) | (UV v) <- vs]
                     $ mkMT m
                     | (vs,i) <- shape_faces s `zip` [0..]
                     ]
        the_normals = fromList $ rawMeshFaceNormals $ RawMesh the_points 
                               $ [ [ pt | Vertex pt _ _ <- vs ] | Face vs _ <- the_faces ]
    
        uvs = shape_uv s


        materialUV :: Material a -> Int -> a
        materialUV (Material _ _ MatchUV) i   = UV i
        materialUV (Material _ _ MatchNoUV) i = ()

example3 = shape cubeShape $ color (1,0,0)
example4 = shape planeShape $ color (1,0.5,0)
example5 = uvShape planeShape $ color (1,0.5,0)
example6 = uvShape planeShape $ uvMaterial "dice"
  [ Kd 1 1 1
  , Map_Kd "dice.jpg"
  , Illum 0
  ]

shape' :: Tessellation (Point V3 Double) -> Material () -> Mesh
shape' tess m = Mesh
          { points  = the_points
          , normals = the_normals
          , uvs     = fromList $ []
          , materials    = fromList $ [ mt | Just mt <- [addNoUVMaterial m] ]
          , uv_materials = fromList $ [ ]
          , faces   = the_faces
          }
  where the_points = T.points tess
        the_faces  = [ Face [Vertex p () (NO i) | p <- [a,b,c]] $ mkMT m
                     | (V3 a b c,i) <- T.faces tess `zip` [0..]
                     ]
        the_normals = fromList $ map (faceNormal (T.points tess)) (T.faces tess)



example7 = shape' (S.plane <$> T.tessellation (V2 1 1))
         $ color (1,0.5,0)

example8 = shape' (S.plane <$> T.tessellation (V2 10 10))
         $ color (1,0.5,0)

example9 = shape' (S.sphere <$> T.tessellation (V2 24 24))
         $ color (1,0.5,0)

-- This has a more general type
uvShape' :: Surface -> Tessellation (Point V2 Double) -> Material a -> Mesh
uvShape' surface tess m = Mesh
          { points  = the_points
          , normals = the_normals
          , uvs     = the_uvs
          , materials    = fromList $ [ mt | Just mt <- [addNoUVMaterial m] ]
          , uv_materials = fromList $ [ mt | Just mt <- [addUVMaterial   m] ]
          , faces   = the_faces
          }
  where the_points = T.points tess'
        the_faces  = [ Face [Vertex p (materialUV m p) (NO i) | p <- [a,b,c]] $ mkMT m
                     | (V3 a b c,i) <- T.faces tess' `zip` [0..]
                     ]
        the_normals = fromList $ map (faceNormal (T.points tess')) (T.faces tess')
        the_uvs = fmap (\ (A.P a) -> a) $ T.points tess
        tess' = surface <$> tess

        materialUV :: Material a -> PT -> a
        materialUV (Material _ _ MatchUV)  (PT i)  = UV i
        materialUV (Material _ _ MatchNoUV) (PT i) = ()

example10 = uvShape' S.sphere (T.tessellation (V2 24 24))
         $ uvMaterial "dice"
              [ Kd 1 1 1
              , Map_Kd "dice.jpg"
              , Illum 0
              ]