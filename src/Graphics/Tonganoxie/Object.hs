{-# LANGUAGE GADTs, KindSignatures, StandaloneDeriving, OverloadedStrings #-}
module Graphics.Tonganoxie.Object where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.List as L
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Vector (Vector, toList, fromList)
import qualified Data.Vector as V

import Linear.Affine (Point, (.+^)) 
import qualified Linear.Affine as A
import Linear.Epsilon
import Linear.Quaternion (Quaternion)
import qualified Linear.Quaternion as Q
import Linear.V3 (V3(V3),cross)
import Linear.V2 (V2(V2))
import Linear.Vector (liftU2)
import Linear.Metric(normalize, distance, dot, qd, Metric)

import System.FilePath (replaceExtension)
import qualified Data.Foldable as F

import Linear.Quaternion.Utils

import Graphics.Tonganoxie.Material 
import Graphics.Tonganoxie.Types

import Debug.Trace

-- To make a Object, you need to choose four things
--  * First, a 'Surface', which is typically represented by a R2 -> R3 function.
--  * Second, a tessellation algorithm, which determines the granularity of the triangles used to create the shape.
--  * Third, the normal fidelity, which can be per-face, per-vertex (implying interpolation),
--     and even finer (via bump map)
--- * Fourth, a material, which may or may not have a UV mapping.

-- A 'Object' is our key data-structure.
data Object = Object 
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

data MT :: * -> * where 
  MTUV   :: Int -> MT UV
  MTNoUV :: Int -> MT ()
  
deriving instance Show (MT a)

data Vertex uv = Vertex !PT uv !NO deriving Show

--------------------------------------------------------------------------------
-- Operations

-- rotate using the given 'Quaternion'. The normals are preserved,
-- because they admit the same rotation.
rotate :: Quaternion Double -> Object -> Object
rotate r m = m
  { points  = fmap (\ (A.P p) -> A.P $ Q.rotate r $ p) (points m)
  , normals = fmap (Q.rotate r) (normals m)
  }
  
-- translate using the given 3D vector. The normals are preserved.
translate :: V3 Double -> Object -> Object
translate dd m = m
  { points  = fmap (.+^ dd) (points m)
  }

-- scale using the given 3D vector. The normals are preserved. 
-- scale by zero makes bad things happen to normals.
scale :: V3 Double -> Object -> Object
scale ss m = m
  { points  = fmap (\ (A.P p) -> A.P $ liftU2 (*) ss p) (points m)
  , normals = fmap (normalize . liftU2 (*) ss') (normals m)
  }
  where ss' = fmap (1/) ss


instance Monoid Object where
    mempty = Object 
           { points    = V.empty 
           , normals   = V.empty
           , uvs       = V.empty
           , materials = V.empty
           , uv_materials = V.empty
           , faces     = []
           }

    mappend m1 m2 = Object 
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

showObject :: Object -> Text
showObject m = T.unlines $
        [ "# generated useing tonganoxie" ] ++
        [ T.unwords $ "v" : map showU [x,y,z] 
        | (A.P (V3 x y z)) <- toList $ points m 
        ] ++
        [ T.unwords $ "vt" : map showU [u,v] 
        | (V2 u v) <- toList $ uvs m 
        ] ++
        [ T.unwords $ "vn" : map showU [x,y,z] 
        | (V3 x y z) <- toList $ normals m 
        ] ++
        [ T.unlines $
          ["usemtl " <> nm] ++
          [ "f " <> T.unwords [ showPT a <> "/" <> showUV mt b <> "/" <> showNO c
                              | Vertex a b c <- vs 
                              ]
          ]
        | Face vs mt <- faces m
        , let nm = case mt of
                     MTUV i   -> materialName $ uv_materials m V.! i
                     MTNoUV i -> materialName $ materials m V.! i
        ] ++
        [ "# end of file" ]
  where
    -- Only show UV indexes if there are any
    showUV :: MT a -> a -> Text
    showUV (MTUV _) (UV i) = T.pack $ show (i + 1)        
    showUV (MTNoUV _)   () = ""

    showPT :: PT -> Text
    showPT (PT i) = T.pack $ show (i + 1)

    showNO :: NO -> Text
    showNO (NO i) = T.pack $ show (i + 1)

-- given foo.obj, writes a foo.obj and foo.mtl file.
writeObject :: FilePath -> Object -> IO ()
writeObject fileName obj = do
    T.writeFile objFileName $ 
            "mtllib " <> T.pack mtlFileName <> "\n" <> showObject obj
    T.writeFile mtlFileName $ T.unlines $
            (map showMaterial $ V.toList $ materials $ obj) ++ 
            (map showMaterial $ V.toList $ uv_materials $ obj) 

  where
    objFileName = fileName
    mtlFileName = replaceExtension fileName "mtl"

-- | 'vertexBasedNormals' adds normals to the vertex.
--   The number of a radian of what constitutes a sharp edge. 
--   The smaller the number, then more sharp edges appear 
--   in the final Object. pi/8 seems a reasonable number.
vertexBasedNormals :: Double -> Object -> Object
vertexBasedNormals angle obj = obj
      { normals      = normals3
      , faces        = [ Face [ Vertex (PT p) uv (rev_map M.! n) 
                              | (Vertex (PT p) uv _,n) <- vs `zip` ns 
                              ] m
                       | (Face vs m,ns) <- faces obj `zip` normals2
                       ]
      }
 where

  -- dot value from comparing normals
  -- 1 == same, 0 == perpendicual, 0.707 == 45 degrees.
  -- So, setting angle to (pi/8) should get the sharp edges.
  edge_threshhold = cos angle
  
  -- select all the normals for each vertex, as a list
  normals1 :: Vector [V3 Double]
  normals1 = V.accum (flip (:)) (fmap (const []) (points obj))
           $ [ (p,normals obj V.! n)
             | Face vs m <- faces obj
             , Vertex (PT p) _ (NO n) <- vs
             ] 

  normals2 :: [[V3 Double]]
  normals2 = [ [ normalize $ sum
                 [ v
                 | v <- normals1 V.! p
                 , let okay = v `dot` no >= edge_threshhold || nearZero ((v `dot` no) - edge_threshhold)
--                 , traceShow (v,no,v `dot` no,edge_threshhold,okay) True
                 , okay
                 ]
               | Vertex (PT p) _ (NO n) <- vs 
               , let no = normals obj V.! n
               ]
             | Face vs m <- faces obj
             ]

  normals3 :: Vector (V3 Double)
  normals3 = V.fromList $ concat normals2

  rev_map :: Map (V3 Double) NO
  rev_map = M.fromList $ zip (V.toList normals3) $ map NO $ [0..]


-- | blend together any points that are really close.
--  Not that this may change the mesh graph.

blend :: Object -> Object
blend mesh = mesh 
      { faces = [ Face (fmap f fs) m | Face fs m <- faces mesh ]
      }
  where
      f :: Vertex a -> Vertex a
      f (Vertex (PT p) uv n) = Vertex (PT (pointsIx V.! p)) uv n
      
      pointsIx :: Vector Int
      pointsIx = fmap ( length
                      . takeWhile not 
                      . fmap nearZero
                      . flip fmap (V.toList $ points mesh)
                      . qd) 
               $ points 
               $ mesh

