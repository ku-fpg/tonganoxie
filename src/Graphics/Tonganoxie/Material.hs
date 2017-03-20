{-# LANGUAGE GADTs, KindSignatures, RankNTypes, StandaloneDeriving, DefaultSignatures, FlexibleInstances, OverloadedStrings #-}
module Graphics.Tonganoxie.Material where

import Data.Char as C
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Graphics.Tonganoxie.Types
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Vector (Vector, toList, fromList)
import qualified Data.Vector as V

import Numeric

--------------------------------------------------------------------------------

data Material :: * -> * where
  Material :: Text -> [Def a] -> Match a -> Material a

deriving instance Show (Material a)
deriving instance Eq (Material a)
deriving instance Ord (Material a)


-- This is just a device for allow GADT matching on UV status; a GADT-friendly boolean.
data Match :: * -> * where
    MatchUV   :: Match UV
    MatchNoUV :: Match ()

deriving instance Show (Match a)
deriving instance Eq (Match a)
deriving instance Ord (Match a)

materialMatch :: Material a -> Match a
materialMatch (Material _ _ m) = m


--------------------------------------------------------------------------------
-- | Summary comments adapted from from http://paulbourke.net/dataformats/mtl/ and
--   https://people.cs.clemson.edu/~dhouse/courses/405/docs/brief-mtl-file-format.html
-- 
--
-- * 'Ka' specifies the ambient reflectivity using RGB values.
-- * 'Kd' specifies the diffuse reflectivity using RGB values.
-- * 'Ks' specifies the specular reflectivity using RGB values.
-- * 'Ns' specifies the specular exponent. A high exponent results in a tight, concentrated highlight. 
--   'Ns' values normally range from 0 to 1000.
-- 
-- * 'Map_Ka' and 'Map_Kd' apply a pointwise map to 'Ka' and 'Kd',
--   using a texture/image.
--    This uses the formula  (given Ka, same for Kd)
--    "Ka - (1-texture alpha) * material ambient + texture alpha * texture value".
--    Assuming an alpha of 1, this reduces to "Ka * texture value".
--
-- * 'Illum' is the illumination model. 
--   '0' is constant color illumination model, via 'Kd'.
--   '1' is diffuse illumination, via 'Ka' and 'Kd', using lighting.
--   '2' is diffuse and specular, via 'Ka', 'Kd', and 'Ks', using lighting.

-- *  'D' specifies the dissolve for the current material, when 1, the default, is full opaque.


data Def :: * -> * where
    Ka :: Double -> Double -> Double -> Def a
    Kd :: Double -> Double -> Double -> Def a
    Ks :: Double -> Double -> Double -> Def a
    Ns :: Double                     -> Def a

    Map_Ka :: Text                   -> Def UV
    Map_Kd :: Text                   -> Def UV

    -- Illumination model
    Illum :: Int                     -> Def a

    D :: Double                      -> Def a

deriving instance Eq (Def a)
deriving instance Ord (Def a)
deriving instance Show (Def a)


materialName :: Material a -> Text
materialName (Material nm _ _) = nm

showMaterial :: Material a -> Text
showMaterial (Material nm defs _) = T.unlines
    ("newmtl " <> nm  : map showDef defs )

showDef :: Def a -> Text
showDef (Ka r g b) = T.unwords ["Ka",showU r,showU g,showU b]
showDef (Kd r g b) = T.unwords ["Kd",showU r,showU g,showU b]
showDef (Ks r g b) = T.unwords ["Ks",showU r,showU g,showU b]
showDef (Ns d)     = T.unwords ["Ns",showU d]
showDef (Map_Ka f) = T.unwords ["map_Ka",f]
showDef (Map_Kd f) = T.unwords ["map_Kd",f]
showDef (Illum i)  = T.unwords ["illum",T.pack $ show i]
showDef (D d)      = T.unwords ["d",showU d]


-- show a number to 5 decimal places.
showU :: Double -> Text
showU d = T.pack $ showFFloat (Just 5) d $ ""

addUVMaterial   :: Material a -> Maybe (Material UV)
addUVMaterial m@(Material _ vs MatchUV)   = Just m
addUVMaterial (Material _ vs MatchNoUV) = Nothing

addNoUVMaterial :: Material a -> Maybe (Material ())
addNoUVMaterial m@(Material _ vs MatchNoUV) = Just m
addNoUVMaterial (Material _ vs MatchUV)   = Nothing

color :: (Double,Double,Double) -> Material ()
color (r,g,b) = material nm [Ka r g b, Kd r g b, Illum 1]
 where nm :: Text
       nm = "rgb_" <> db r <> db g <> db b
       db :: Double -> Text
       db d = T.pack $ reverse $ take 2 $ reverse $ ("0" ++ showHex (round (d * 255)) "")
       
uvMaterial :: Text -> [Def UV] -> Material UV
uvMaterial nm defs = Material nm defs MatchUV

material :: Text -> [Def ()] -> Material ()
material nm defs = Material nm defs MatchNoUV

mapUV :: Material a -> Material UV
mapUV (Material nm defs _) = uvMaterial nm (map mUV defs) 
  where
        mUV :: Def a -> Def UV
        mUV (Ka r g b) = Ka r g b
        mUV (Kd r g b) = Kd r g b
        mUV (Ks r g b) = Ks r g b
        mUV (Ns d)     = Ns d
        mUV (Map_Ka f) = Map_Ka f
        mUV (Map_Kd f) = Map_Kd f
        mUV (Illum i)  = Illum i
        mUV (D d)      = D d

------------------------------------------------------------
data Materials :: * where
  Materials :: [Material ()] -> [Material UV] -> Materials

deriving instance Show Materials

-- | Material files are mappings from material names to materials.
readMaterials :: FilePath -> IO Materials
readMaterials fileName = do
    txt <- readFile fileName
    return $ mtlParser $ txt
    
data MtlLexeme 
  = MtlNew Text
  | MtlDef   (forall a . Def a)
  | MtlDefUV (Def UV)
  
deriving instance Show MtlLexeme  
  
mtlLexer :: String -> [MtlLexeme]
mtlLexer = concatMap lexer . map words . lines 
  where
      lexer :: [String] -> [MtlLexeme]
      lexer ["newmtl",mtl] = [MtlNew (T.pack mtl)]
      lexer ["Ka",r,g,b] = [MtlDef $ Ka (read r) (read g) (read b)]
      lexer ["Kd",r,g,b] = [MtlDef $ Kd (read r) (read g) (read b)]
      lexer ["map_Kd",f] = [MtlDefUV $ Map_Kd (T.pack f)]
      lexer ["illum",i]  = [MtlDef $ Illum (read i)]
      lexer [] = []
      lexer ("#":_) = []
      lexer ln = error $ ".mtl lexer error " ++ show ln

mtlParser :: String -> Materials
mtlParser = find (Materials [] []) . mtlLexer
  where
      find :: Materials -> [MtlLexeme] -> Materials
      find ms (MtlNew nm : rest) = findU ms (material nm []) rest
      find ms (lx : _) = error $ "extra text before material name" ++ show lx
      find ms [] = ms

      findU :: Materials -> Material () -> [MtlLexeme] -> Materials
      findU ms (Material nm defs mx) (MtlDef d : rest) = 
          findU ms (Material nm (defs ++ [d]) mx) rest
      findU ms m (MtlDefUV d : rest) = case mapUV m of
          Material nm defs mx -> findUV ms (Material nm (defs ++ [d]) mx) rest
      findU ms m rest = find (case ms of
           Materials ms ms_uv -> Materials (ms ++ [m]) ms_uv) rest
          
      findUV :: Materials -> Material UV -> [MtlLexeme] -> Materials
      findUV ms (Material nm defs mx) (MtlDef d : rest) = 
          findUV ms (Material nm (defs ++ [d]) mx) rest
      findUV ms (Material nm defs mx) (MtlDefUV d : rest) = 
          findUV ms (Material nm (defs ++ [d]) mx) rest
      findUV ms m rest = find (case ms of
           Materials ms ms_uv -> Materials ms (ms_uv ++ [m])) rest

writeMaterial :: FilePath -> Materials -> IO ()
writeMaterial fileName (Materials mts uv_mts) = do
    T.writeFile fileName $ T.unlines $
            (map showMaterial $ mts) ++ 
            (map showMaterial $ uv_mts) 

