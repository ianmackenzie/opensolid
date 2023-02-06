module Units
  ( Coercion
  , drop
  , add
  , map
  , map2
  , map3
  , map4
  , mapT
  , mapT2
  , mapT3
  , mapT4
  , call
  , call2
  , call3
  , call4
  , callT
  , callT2
  , callT3
  , callT4
  )
where

import Generic (Units)
import OpenSolid
import Unsafe.Coerce (unsafeCoerce)

class Coercion (a :: Type -> Type)

instance Coercion Qty

drop :: Coercion a => a units -> a Unitless
drop = unsafeCoerce

add :: Coercion a => a Unitless -> a units
add = unsafeCoerce

toGeneric :: Coercion a => a units -> a Units
toGeneric = unsafeCoerce

fromGeneric :: Coercion a => a Units -> a units
fromGeneric = unsafeCoerce

map :: (Coercion a, Coercion b) => (a Units -> b Units) -> a units -> b units
map function a =
  fromGeneric (function (toGeneric a))

map2 :: (Coercion a, Coercion b, Coercion c) => (a Units -> b Units -> c Units) -> a units -> b units -> c units
map2 function a b =
  fromGeneric (function (toGeneric a) (toGeneric b))

map3 :: (Coercion a, Coercion b, Coercion c, Coercion d) => (a Units -> b Units -> c Units -> d Units) -> a units -> b units -> c units -> d units
map3 function a b c =
  fromGeneric (function (toGeneric a) (toGeneric b) (toGeneric c))

map4 :: (Coercion a, Coercion b, Coercion c, Coercion d, Coercion e) => (a Units -> b Units -> c Units -> d Units -> e Units) -> a units -> b units -> c units -> d units -> e units
map4 function a b c d =
  fromGeneric (function (toGeneric a) (toGeneric b) (toGeneric c) (toGeneric d))

mapT :: (Tolerance units, Coercion a, Coercion b) => (Tolerance Units => a Units -> b Units) -> a units -> b units
mapT function a =
  fromGeneric (function (toGeneric a))
 where
  ?tolerance = toGeneric ?tolerance

mapT2 :: (Tolerance units, Coercion a, Coercion b, Coercion c) => (Tolerance Units => a Units -> b Units -> c Units) -> a units -> b units -> c units
mapT2 function a b =
  fromGeneric (function (toGeneric a) (toGeneric b))
 where
  ?tolerance = toGeneric ?tolerance

mapT3 :: (Tolerance units, Coercion a, Coercion b, Coercion c, Coercion d) => (Tolerance Units => a Units -> b Units -> c Units -> d Units) -> a units -> b units -> c units -> d units
mapT3 function a b c =
  fromGeneric (function (toGeneric a) (toGeneric b) (toGeneric c))
 where
  ?tolerance = toGeneric ?tolerance

mapT4 :: (Tolerance units, Coercion a, Coercion b, Coercion c, Coercion d, Coercion e) => (Tolerance Units => a Units -> b Units -> c Units -> d Units -> e Units) -> a units -> b units -> c units -> d units -> e units
mapT4 function a b c d =
  fromGeneric (function (toGeneric a) (toGeneric b) (toGeneric c) (toGeneric d))
 where
  ?tolerance = toGeneric ?tolerance

call :: Coercion a => (a Units -> b) -> a units -> b
call function a =
  function (toGeneric a)

call2 :: (Coercion a, Coercion b) => (a Units -> b Units -> c) -> a units -> b units -> c
call2 function a b =
  function (toGeneric a) (toGeneric b)

call3 :: (Coercion a, Coercion b, Coercion c) => (a Units -> b Units -> c Units -> d) -> a units -> b units -> c units -> d
call3 function a b c =
  function (toGeneric a) (toGeneric b) (toGeneric c)

call4 :: (Coercion a, Coercion b, Coercion c, Coercion d) => (a Units -> b Units -> c Units -> d Units -> e) -> a units -> b units -> c units -> d units -> e
call4 function a b c d =
  function (toGeneric a) (toGeneric b) (toGeneric c) (toGeneric d)

callT :: (Tolerance units, Coercion a) => (Tolerance Units => a Units -> b) -> a units -> b
callT function a =
  function (toGeneric a)
 where
  ?tolerance = toGeneric ?tolerance

callT2 :: (Tolerance units, Coercion a, Coercion b) => (Tolerance Units => a Units -> b Units -> c) -> a units -> b units -> c
callT2 function a b =
  function (toGeneric a) (toGeneric b)
 where
  ?tolerance = toGeneric ?tolerance

callT3 :: (Tolerance units, Coercion a, Coercion b, Coercion c) => (Tolerance Units => a Units -> b Units -> c Units -> d) -> a units -> b units -> c units -> d
callT3 function a b c =
  function (toGeneric a) (toGeneric b) (toGeneric c)
 where
  ?tolerance = toGeneric ?tolerance

callT4 :: (Tolerance units, Coercion a, Coercion b, Coercion c, Coercion d) => (Tolerance Units => a Units -> b Units -> c Units -> d Units -> e) -> a units -> b units -> c units -> d units -> e
callT4 function a b c d =
  function (toGeneric a) (toGeneric b) (toGeneric c) (toGeneric d)
 where
  ?tolerance = toGeneric ?tolerance
