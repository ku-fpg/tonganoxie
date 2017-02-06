{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, GeneralizedNewtypeDeriving, TypeFamilies #-}
import Linear.V3 (V3(..),cross)
import Linear.Vector
import Linear.Affine
import Linear.Metric
import Linear.Quaternion
import Linear.Matrix as M
import Linear.Epsilon

import qualified Data.List as List
import qualified Data.Text as T

import Debug.Trace as D

import Test.QuickCheck
import Control.Applicative

-- imports from the package under test
import Linear.Quaternion.Utils

main :: IO ()
main = do
     let large = 1000000
     quickCheckWith stdArgs { maxSuccess = large }    prop_betweenq
     quickCheckWith stdArgs { maxSuccess = large }    prop_q2e2q
     quickCheck                                       prop_invert
     quickCheck                                       prop_reorder
     quickCheckWith stdArgs { maxSuccess = large }    prop_orthogonal 
     putStrLn "Success!"

------------------------------------------------------------------------------

prop_orthogonal (A'V3 v) =
    not (nearZero v) ==> nearZero (dot (normalize v) (normalize (orthogonal v)))


------------------------------------------------------------------------------


prop_betweenq (A'V3 v1) (A'V3 v2) =
    not (nearZero v1 || nearZero v2) ==>
    nearZero $ test_betweenq (normalize v1) (normalize v2)

test_betweenq v1 v2 = rotate (betweenq v1 v2) v1 - v2

------------------------------------------------------------------------------

prop_q2e2q (A'Order o) (A'Quaternion q) = eqQ (test_q2e2q o q) q
test_q2e2q o = eulerToQuaternion . quaternionToEuler o

eqQ :: (Epsilon a, RealFloat a) => Quaternion a -> Quaternion a -> Bool
eqQ qa qb = nearZero (qa - qb) || nearZero (qa + qb)

------------------------------------------------------------------------------
-- Momomophic for testing
newtype A'V3 = A'V3 (V3 Double)
  deriving Show

instance Arbitrary A'V3 where
    arbitrary = A'V3 <$> (V3 <$> a <*> a <*> a)
     where a = oneof [pure 0, pure 1, pure (-1), arbitrary]


-- Momomophic for testing, only generates unit 'Quaternion's.
newtype A'Quaternion = A'Quaternion (Quaternion Double)
  deriving Show

instance Arbitrary A'Quaternion where
    arbitrary = do
    	      A'V3 v <- arbitrary
	      if nearZero v
	      then arbitrary -- re-try
	      else do r <- oneof $ arbitrary 
	      	      	         : [pure (n*m) 
				   | n <- [-1,1]
				   , m <- [0,pi/4,pi/3,pi/2,pi]
				   ]
	      	      return $ A'Quaternion $ axisAngle v r


newtype A'Order = A'Order Order
  deriving Show

instance Arbitrary A'Order where
    arbitrary = A'Order <$> elements 
        [ XYZ
        , XZY
        , YXZ
        , YZX
        , ZXY
        , ZYX
        ]


data V = V String | Z | One | Neg V | Plus V V | Times V V
    deriving Eq

instance Show V where
    show (V n) = n
    show Z     = "0"
    show One   = "1"
    show (Neg v) = "-" ++ show v
    show (Plus v1 v2) = show v1 ++ "+" ++ show v2
    show (Times v1 v2) = show v1 ++ "*" ++ show v2

instance Num V where
    Z + n = n
    n + Z = n
    n + m = Plus n m
    
    Z * _ = Z
    _ * Z = Z
    One * n = n
    n * One = n
    (Neg n) * (Neg m) = n * m
    n * (Neg m) = Neg (n * m)
    Neg n * m = Neg (n * m)
    n * m = Times n m

    fromInteger 0 = Z
    fromInteger 1 = One
    fromInteger n = if n < 0 then Neg (negate (V $ show n)) else (V $ show n)
    

    negate Z       = Z
    negate (Neg n) = n
    negate n       = Neg n

m :: V3 (V3 V)
m = fmap (fmap V)
  $ V3 (V3 "m11" "m12" "m13")
       (V3 "m21" "m22" "m23")
       (V3 "m31" "m32" "m33")

m' = fmap (fmap V)
  $ V3 (V3 "+m22" "-m21" "-m23")
       (V3 "_"    "+m11" "-m13")
       (V3 "_"    "-m31" "+m33")


vX = V3 1 0 0 :: V3 V
vY = V3 0 1 0 :: V3 V
vZ = V3 0 0 1 :: V3 V

_1 (V3 x _ _) = x
_2 (V3 _ x _) = x
_3 (V3 _ _ x) = x

bad o (q :: Quaternion Double) = do
    print o
    print q
    let e = quaternionToEuler o q
    print e
    let q' = eulerToQuaternion e
    print q'
    
---------

prop_invert (A'Order o) = invert (invert o) == o
    
prop_reorder (A'Order o) x y z = reorder (invert o) (reorder o v) == v 
  where v = V3 x y z :: V3 Int