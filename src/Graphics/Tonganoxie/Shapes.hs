{-# LANGUAGE GADTs, KindSignatures, StandaloneDeriving, OverloadedStrings #-}
module Graphics.Tonganoxie.Shapes where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Foldable(Foldable)
import qualified Data.Foldable as F
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
import Graphics.Tonganoxie.Object
import Graphics.Tonganoxie.Normals
import qualified Graphics.Tonganoxie.Surface as S
import Graphics.Tonganoxie.Types


import Graphics.Tonganoxie.Tessellation(Mesh)
import qualified Graphics.Tonganoxie.Tessellation as T

import Linear.Epsilon


mkMT :: Material a -> MT a
mkMT (Material _ _ MatchUV)   = MTUV 0
mkMT (Material _ _ MatchNoUV) = MTNoUV 0

           

shape :: Foldable g => Mesh g R3 -> Material () -> Object
shape tess m = Object
          { points  = the_points
          , normals = the_normals
          , uvs     = fromList $ []
          , materials    = fromList $ [ mt | Just mt <- [addNoUVMaterial m] ]
          , uv_materials = fromList $ [ ]
          , faces   = the_faces
          }
  where the_points = T.points tess
        the_faces  = [ Face [Vertex p () (NO i) | p <- F.toList g] $ mkMT m
                     | (g,i) <- T.faces tess `zip` [0..]
                     ]
        the_normals = fromList $ map (faceNormal (T.points tess)) (T.faces tess)

example7 = shape (S.plane <$> T.tessellation (V2 1 1))
         $ color (1,0.5,0)

example8 = shape (S.plane <$> T.tessellation (V2 10 10))
         $ color (1,0.5,0)

example9' = shape (S.sphere <$> T.tessellation (V2 4 4))
         $ color (1,0.5,0)

example9'' = shape (S.sphere <$> T.tessellation (V2 10 10))
         $ color (1,0.5,0)

example9 = shape (S.sphere <$> T.tessellation (V2 24 24))
         $ color (1,0.5,0)

-- | uvShape takes a 'Surface' and a 'Object', combines them together,
--  and adds a 'Material'. The UV values of the 'Surface' are
--  extracted.
--
--     uvShape s t m = shape (s <$> t) m, if you ignore the UV values;
--
--

uvShape :: Foldable g => Surface -> Mesh g R2 -> Material a -> Object
uvShape surface tess m = Object
          { points  = the_points
          , normals = the_normals
          , uvs     = the_uvs
          , materials    = fromList $ [ mt | Just mt <- [addNoUVMaterial m] ]
          , uv_materials = fromList $ [ mt | Just mt <- [addUVMaterial   m] ]
          , faces   = the_faces
          }
  where the_points = T.points tess'
        the_faces  = [ Face [Vertex p (materialUV m p) (NO i) | p <- F.toList g] $ mkMT m
                     | (g,i) <- T.faces tess' `zip` [0..]
                     ]
        the_normals = fromList $ map (faceNormal (T.points tess')) (T.faces tess')
        the_uvs = fmap (\ (A.P a) -> a) $ T.points tess
        tess' = surface <$> tess

        materialUV :: Material a -> PT -> a
        materialUV (Material _ _ MatchUV)  (PT i)  = UV i
        materialUV (Material _ _ MatchNoUV) (PT i) = ()

example10 = uvShape S.sphere (T.tessellation (V2 12 12))
         $ uvMaterial "dice"
              [ Kd 1 1 1
              , Map_Kd "dice.jpg"
              , Illum 0
              ]

example11 = shape (T.cubeMesh)
         $ color (1,0.5,0)
