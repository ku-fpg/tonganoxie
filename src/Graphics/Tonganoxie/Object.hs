{-# LANGUAGE GADTs, KindSignatures, StandaloneDeriving, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Graphics.Tonganoxie.Object where

import qualified Codec.Wavefront.IO as WF
import qualified Codec.Wavefront    as WF

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Monoid ((<>))
import Data.Text (Text)
--import Data.List as L
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Vector (Vector, toList)
import qualified Data.Vector as V

import Linear.Affine (Point, (.+^)) 
import qualified Linear.Affine as A
import Linear.Epsilon
import Linear.Quaternion (Quaternion)
import qualified Linear.Quaternion as Q
import Linear.V3 (V3(V3))
import Linear.V2 (V2(V2))
import Linear.Vector (liftU2)
import Linear.Metric(normalize, dot, qd)

import System.FilePath (replaceExtension)
--import qualified Data.Foldable as F

--import Linear.Quaternion.Utils hiding (x,y,z)

import Graphics.Tonganoxie.Material
import Graphics.Tonganoxie.Types
import Graphics.Tonganoxie.Normals

--import Debug.Trace

import GHC.Float (float2Double)

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
  show (Face vs mt@(MTIgUV _)) = "Face " ++ show vs ++ " " ++ show mt
  
data MT :: * -> * where 
  MTUV   :: Int -> MT UV -- use a UV-based material with a UV mesh
  MTNoUV :: Int -> MT () -- use a non-UV-based material with a non-UV mesh
  MTIgUV :: Int -> MT UV -- use a non-UV-based material with a **UV mesh**
                         -- There is no 4th case; this is what we want to avoid

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
           incUV (MTUV   _)   (UV i) = UV (i + uvOff)
           incUV (MTNoUV _)   ()     = ()
           incUV (MTIgUV   _) (UV i) = UV (i + uvOff)

           incM :: MT uv -> MT uv
           incM (MTUV   i) = MTUV   (i + uv_mOff)
           incM (MTNoUV i) = MTNoUV (i + mOff)
           incM (MTIgUV i) = MTIgUV (i + mOff) -- The material non-UV

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
                     MTIgUV i -> materialName $ materials m V.! i

        ] ++
        [ "# end of file" ]
  where
    -- Only show UV indexes if there are any
    showUV :: MT a -> a -> Text
    showUV (MTUV _)   (UV i) = T.pack $ show (i + 1)        
    showUV (MTNoUV _)     () = ""
    showUV (MTIgUV _) (UV i) = T.pack $ show (i + 1)        

    showPT :: PT -> Text
    showPT (PT i) = T.pack $ show (i + 1)

    showNO :: NO -> Text
    showNO (NO i) = T.pack $ show (i + 1)

-- given foo.obj, writes a foo.obj and foo.mtl file.
writeObject :: FilePath -> Object -> IO ()
writeObject fileName obj = do
    T.writeFile objFileName $ 
            "mtllib " <> T.pack mtlFileName <> "\n" <> showObject obj
    writeMaterial mtlFileName $ Materials
            (V.toList $ materials $ obj)
            (V.toList $ uv_materials $ obj) 
  where
    objFileName = fileName
    mtlFileName = replaceExtension fileName "mtl"


readObject :: FilePath -> IO Object
readObject fileName = do
    wfr <- WF.fromFile fileName
    case wfr of
      Left msg -> fail $ show msg
      Right wf -> do
        -- next, read the materials
        mss <- mapM readMaterials $ fmap T.unpack $ V.toList $ WF.objMtlLibs wf
        let msU = 
                [ m 
                | Materials ms _ <- mss
                , m <- ms
                ]
        let msU_map :: Map Text (MT ())
            msU_map = M.fromList [ (nm,MTNoUV ix)
                                 | (ix,Material nm _ _) <- [0..] `zip` msU 
                                 ]
            
        let msUV =
                [ m 
                | Materials _ ms <- mss
                , m <- ms
                ] 

        let msUV_map :: Map Text (MT UV)
            msUV_map = M.fromList [ (nm,MTUV ix)
                                  | (ix,Material nm _ _) <- [0..] `zip` msUV 
                                  ]
        print wf
        print (msU,msUV)
        let all_points = fmap (fmap float2Double)
                              $ fmap (\ (WF.Location x y z _) -> A.P $ V3 x y z) 
                              $ WF.objLocations wf

        -- wfFaceNormal                      
        let wfFaceNormal :: WF.Element WF.Face -> V3 Double
            wfFaceNormal (WF.Element _ _ _ _ (WF.Face p1 p2 p3 _)) = 
                faceNormal all_points [PT (i-1) | WF.FaceIndex i _ _ <- [p1,p2,p3]]

        let all_normals = ( fmap (fmap float2Double)
                            $ fmap (\ (WF.Normal x y z) -> V3 x y z) 
                            $ WF.objNormals wf
                            ) V.++
                            -- We explicitly add computed (default) normals
                            -- for all faces. The 'gc' can clean this up, if needed.
                            -- This is because we require normals in our representation.
                            (fmap wfFaceNormal $ WF.objFaces wf)

        let face_normal_offset = V.length $ WF.objNormals wf

        let parseFaceIndexUV :: NO -> WF.FaceIndex -> Maybe (Vertex UV)
            parseFaceIndexUV def_no (WF.FaceIndex pt_ix opt_uv opt_no) = do
                    let pt = PT (pt_ix-1)
                    uv <- fmap (UV . pred) opt_uv -- This is the point that can fail
                    let no = case opt_no of
                               Just no_ix -> NO (no_ix - 1)
                               Nothing    -> def_no
                    return $ Vertex pt uv no

        let parseFaceIndexNoUV :: NO -> WF.FaceIndex -> Maybe (Vertex ())
            parseFaceIndexNoUV def_no (WF.FaceIndex pt_ix opt_uv opt_no) = do
                    let pt = PT (pt_ix-1)
                    uv <- case opt_uv of
                            Nothing -> return ()
                            Just _ -> fail "UV found when none was expected"
                    let no = case opt_no of
                               Just no_ix -> NO (no_ix-1)
                               Nothing    -> def_no
                    return $ Vertex pt uv no

        let parseFaceUV :: NO -> WF.Element WF.Face -> Maybe Face
            parseFaceUV no (WF.Element _ _ optMtl _ (WF.Face i1 i2 i3 is)) = do
                face_vs <- sequence [ parseFaceIndexUV no i| i <- [i1,i2,i3] ++ is]
                mtl_nm <- optMtl
                mt <- case M.lookup mtl_nm msUV_map of
                        Nothing -> case M.lookup mtl_nm msU_map of
                                     Just (MTNoUV mt') -> return (MTIgUV mt')
                                     Just _ -> error "internal error"
                                     Nothing -> fail "not found material"
                        Just mt' -> return mt'                  
                return $ Face face_vs mt

            parseFaceNoUV :: NO -> WF.Element WF.Face -> Maybe Face
            parseFaceNoUV no (WF.Element _ _ optMtl _ (WF.Face i1 i2 i3 is)) = do
                face_vs <- sequence [ parseFaceIndexNoUV no i| i <- [i1,i2,i3] ++ is]
                mtl_nm <- optMtl
                mt <- M.lookup mtl_nm msU_map
                return $ Face face_vs mt


        return $ Object 
               { points       = all_points
               , normals      = all_normals
               , uvs          = fmap (fmap float2Double)
                              $ fmap (\ (WF.TexCoord u v _) ->  V2 u v)
                              $ WF.objTexCoords wf
               , materials    = V.fromList msU
               , uv_materials = V.fromList msUV
               , faces        = 
                              [ case parseFaceUV (NO ix) f of
                                  Just f_UV -> f_UV
                                  Nothing -> case parseFaceNoUV (NO ix) f of
                                    Just f_noUV -> f_noUV
                                    Nothing -> error "no parse for a face"
                              | (ix,f) <- [face_normal_offset..] `zip` (V.toList (WF.objFaces wf))
                              ]
               }


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
             | Face vs _ <- faces obj
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
             | Face vs _ <- faces obj
             ]

  normals3 :: Vector (V3 Double)
  normals3 = V.fromList $ concat normals2

  rev_map :: Map (V3 Double) NO
  rev_map = M.fromList $ zip (V.toList normals3) $ map NO $ [0..]


-- | blend together any points that are really close.
--  Note that this may change the mesh graph.

blendMesh :: Object -> Object
blendMesh mesh = mesh 
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

-- 'gc' removes any unused points, uv's, normals, or materials.
-- It is easier to utilize 'gc', rather than try make sure all
-- vectors inside object are still live after a transformation.

gc :: Object -> Object
gc = id
