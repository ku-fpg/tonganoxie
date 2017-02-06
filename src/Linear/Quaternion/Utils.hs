module Linear.Quaternion.Utils where

import Linear.Affine
import Linear.Epsilon
import Linear.Matrix as M
import Linear.Metric
import Linear.Quaternion
import Linear.V3 (V3(..),cross)
import Linear.Vector

import qualified Debug.Trace as D

{- | This is an alternative to the Quaternion format,
     following the THREE.js representation of Euler.
     These are intrinsic Tait–Bryan rotations.

From THREE.js documentation:

"The order in which to apply rotations. Default is 'XYZ',
which means that the object will first be rotated around its
X axis, then its Y axis and finally its Z axis. Other
possibilities are: 'YZX', 'ZXY', 'XZY', 'YXZ' and 'ZYX'.
These must be in upper case.

Three.js uses intrinsic (Tait-Bryan) ordering, also known as
yaw, pitch and roll. This means that rotations are performed
with respect to the local coordinate system. That is, for
order 'XYZ', the rotation is first around world-X, then
around local-Y (which may now be different from the world
Y-axis), then local-Z (which may be different from the world
Z-axis).

Some implementations may use extrinsic (proper) ordering, in
which case rotations are performed with respect to the world
coordinate system, so that for order 'XYZ', the rotations are
around world-X, world-Y, and world-Z.

Converting between the two types is relatively
straightforward, you just need to reverse the order and the
rotation, so that an intrinsic (three.js) Euler rotation of
angles a, b, c about XYZ will be equivalent to to an
extrinsic Euler rotation of angles c, b, a about ZYX."

-}

data Euler a = Euler 
     { order :: Order
     , x :: a
     , y :: a
     , z :: a
     }
     deriving (Eq, Ord, Show, Read)

data Order = XYZ | XZY | YXZ | YZX | ZXY | ZYX
     deriving (Eq, Ord, Show, Read)

{-# RULES "reorder" forall o v . reorder (invert o) (reorder o v) = v  #-}

-- | Reorder a V3, assuming X-Y-Z, into the given order.
--
-- RULE: reorder (invert o) (reorder o v) = v
--
reorder :: Order -> V3 a -> V3 a
reorder XYZ (V3 x y z) = V3 x y z
reorder XZY (V3 x y z) = V3 x z y
reorder YXZ (V3 x y z) = V3 y x z
reorder YZX (V3 x y z) = V3 y z x
reorder ZXY (V3 x y z) = V3 z x y
reorder ZYX (V3 x y z) = V3 z y x


{-# RULES "invert" forall o . invert (invert o) = o #-}

-- | This inverts the order. Think of 'Order'
-- as a maping; invert inverts the mapping.
-- When used in conjuction with reorder, 
-- the reorder performed
-- will be the inverse, compared to using
-- reorder without invert. 
--
-- RULE: invert (invert o) = o
--
invert :: Order -> Order
invert XYZ = XYZ
invert XZY = XZY
invert YXZ = YXZ
invert YZX = ZXY
invert ZXY = YZX
invert ZYX = ZYX
{-
     vX = V3 1 0 0
     vY = V3 0 1 0
     vZ = V3 0 0 1
-}

    

-- | Convert a 'Quaternion' into a (ordered) 'Euler'.
quaternionToEuler :: (Epsilon a, RealFloat a) => Order -> Quaternion a -> Euler a
quaternionToEuler o q = Euler o rX rY rZ
  where
     -- adapted from https://github.com/mrdoob/three.js/blob/master/src/math/Euler.js
     V3 (V3 m11 m12 m13)
        (V3 m21 m22 m23)
        (V3 m31 m32 m33) = reorder o $ transpose $ reorder o $ transpose $ fromQuaternion q

     V3 a1 a2 a3 = reorder o $ V3 xAxis yAxis zAxis :: V3 (V3 Int)
     -- Does order reflect right-hand rule, or does it need inverted.
     sgn x = if cross a1 a2 == a3 then x else -x

     V3 rX rY rZ = sgn $ reorder (invert o) $ V3 r1 r2 r3
       where
         gimbal = not $ nearZero $ 1 - abs m13
         r2 = asin $ max (-1) $ min m13 $ 1
         r1 | gimbal    = atan2 (-m23) m33
            | otherwise = atan2   m32  m22
         r3 | gimbal    = atan2 (-m12) m11
            | otherwise = 0
     
{-
    function f(w,x,y,z,order) {
      var q = new THREE.Quaternion(x,y,z,w);
      var e = new THREE.Euler(0,0,0,order);
      e.setFromQuaternion(q);
      return e;
-} 

-- Adapted from http://stackoverflow.com/questions/1171849/finding-quaternion-representing-the-rotation-from-one-vector-to-another

-- | Take two vectors, and figure a 'Quaternion' that rotates between them.
--   The result 'Quaternion' might not be unique.
betweenq :: (RealFloat a, Show a, Epsilon a, Floating a) => V3 a -> V3 a -> Quaternion a
betweenq v1 v2 
      -- for 180 rotation, rotate around any orthogonal vector
    | nearZero (d + 1) = axisAngle (orthogonal n_v1) pi
    | otherwise        = axisAngle c (acos d)
  where
    n_v1 = normalize v1
    n_v2 = normalize v2
    c = cross n_v1 n_v2
    d = max (-1) $ min 1 $ dot n_v1 n_v2
    ca = acos d

-- for a slightly better solution

orthogonal :: (Ord a, Num a) => V3 a -> V3 a
orthogonal v = cross v other
  where
    V3 x y z = abs v
    other | x < y && x < z = xAxis
          | x < y          = zAxis
          | y < z          = yAxis
          | otherwise      = zAxis

        
-- map a 'Euler' back into a 'Quaternion'
eulerToQuaternion :: (Epsilon a, RealFloat a) => Euler a -> Quaternion a
eulerToQuaternion (Euler o x y z) = product $ reorder o (V3 _x _y _z)
  where
      _x = axisAngle xAxis x 
      _y = axisAngle yAxis y 
      _z = axisAngle zAxis z

-- Normals along specific axis.
xAxis, yAxis, zAxis :: Num a => V3 a
xAxis = V3 1 0 0
yAxis = V3 0 1 0
zAxis = V3 0 0 1
