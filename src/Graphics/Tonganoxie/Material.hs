{-# LANGUAGE GADTs, KindSignatures, RankNTypes, StandaloneDeriving #-}
module Graphics.Tonganoxie.Material where

import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T

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

--  Texture  :: Text -> Material UV

newtype UV = UV Int deriving Show

materialMatch :: Material a -> Match a
materialMatch (Material _ _ m) = m

{-
showUV :: Material uv -> uv -> String
showUV (Material _ _ MatchNoUV)  NoUV   = ""
--showUV (Texture {}) (UV i) = show (i + 1)

materialUV :: Material uv -> Int -> uv
materialUV (Material) _   = NoUV
materialUV (Texture {}) i = UV i

incUV :: Material uv -> Int -> uv -> uv
incUV (Material)   _ NoUV   = NoUV
incUV (Texture {}) n (UV i) = UV (n + i)

materialName :: Material uv -> String
materialName (Material) = "color"
-}

--------------------------------------------------------------------------------
-- | Summary comments from http://paulbourke.net/dataformats/mtl/

data Def :: * -> * where
    -- | The Ka statement specifies the ambient reflectivity using RGB values.
    Ka :: Double -> Double -> Double -> Def a
    --  | The Kd statement specifies the diffuse reflectivity using RGB values.
    Kd :: Double -> Double -> Double -> Def a
    -- | The Ks statement specifies the specular reflectivity using RGB values.
    Ks :: Double -> Double -> Double -> Def a
    Map_Ka :: Text                   -> Def UV

deriving instance Eq (Def a)
deriving instance Ord (Def a)
deriving instance Show (Def a)

eqDef :: Def a -> Def b -> Bool
eqDef (Ka r g b) (Ka r' g' b') = r == r' && g == g' && b == b'
eqDef _ _ = False


showMaterial :: Material a -> Text
showMaterial (Material nm defs _) = T.unlines
    ("newmtl " <> nm  : map showDef defs )

showDef :: Def a -> Text
showDef (Ka r g b) = T.unwords ["Ka",showU r,showU g,showU b]


-- show unit value (between 0 and 1, inclusive)
showU :: Double -> Text
showU d = T.pack $ showFFloat (Just 5) d $ ""

addUVMaterial   :: Material a -> Maybe (Material UV)
addUVMaterial m@(Material _ vs MatchUV)   = Just m
addUVMaterial (Material _ vs MatchNoUV) = Nothing

addNoUVMaterial :: Material a -> Maybe (Material ())
addNoUVMaterial m@(Material _ vs MatchNoUV) = Just m
addNoUVMaterial (Material _ vs MatchUV)   = Nothing

color :: (Double,Double,Double) -> Material ()
color (r,g,b) = Material nm [Ka r g b] MatchNoUV
 where nm = "rgb_" <> db r <> db g <> db b
 
       db :: Double -> Text
       db d = T.pack $ reverse $ take 2 $ reverse $ ("0" ++ showHex (round (d * 255)) "")