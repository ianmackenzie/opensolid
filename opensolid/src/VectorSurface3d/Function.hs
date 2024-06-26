module VectorSurface3d.Function
  ( Function
  , Interface (..)
  , wrap
  , zero
  , constant
  , xyz
  , evaluate
  , bounds
  )
where

import OpenSolid
import Surface1d qualified
import Surface1d.Function qualified
import Units (Erase)
import Units qualified
import Uv (Parameter)
import Uv qualified
import Vector3d (Vector3d)
import Vector3d qualified
import VectorBounds3d (VectorBounds3d)
import VectorBounds3d qualified

class
  Show function =>
  Interface function (coordinateSystem :: CoordinateSystem)
    | function -> coordinateSystem
  where
  evaluateImpl :: function -> Uv.Point -> Vector3d coordinateSystem
  boundsImpl :: function -> Uv.Bounds -> VectorBounds3d coordinateSystem
  derivativeImpl :: Parameter -> function -> Function coordinateSystem

data Function (coordinateSystem :: CoordinateSystem) where
  Function ::
    Interface function (space @ units) =>
    function ->
    Function (space @ units)
  Coerce ::
    Function (space @ units1) ->
    Function (space @ units2)
  Constant ::
    Vector3d (space @ units) ->
    Function (space @ units)
  XYZ ::
    Surface1d.Function units ->
    Surface1d.Function units ->
    Surface1d.Function units ->
    Function (space @ units)
  Negated ::
    Function (space @ units) ->
    Function (space @ units)
  Sum ::
    Function (space @ units) ->
    Function (space @ units) ->
    Function (space @ units)
  Difference ::
    Function (space @ units) ->
    Function (space @ units) ->
    Function (space @ units)
  Product1d3d' ::
    Surface1d.Function units1 ->
    Function (space @ units2) ->
    Function (space @ (units1 :*: units2))
  Product3d1d' ::
    Function (space @ units1) ->
    Surface1d.Function units2 ->
    Function (space @ (units1 :*: units2))
  Quotient' ::
    Function (space @ units1) ->
    Surface1d.Function units2 ->
    Function (space @ (units1 :/: units2))

deriving instance Show (Function (space @ units))

instance HasUnits (Function (space @ units)) where
  type Units (Function (space @ units)) = units
  type Erase (Function (space @ units)) = Function (space @ Unitless)

instance
  space ~ space_ =>
  Units.Coercion (Function (space @ units1)) (Function (space_ @ units2))
  where
  coerce function = case function of
    Constant v -> Constant (Units.coerce v)
    Coerce f -> Coerce f
    _ -> Coerce function

instance Negation (Function (space @ units)) where
  negate function = case function of
    Coerce f -> Coerce -f
    Constant v -> Constant -v
    XYZ x y z -> XYZ -x -y -z
    Negated f -> f
    Difference f1 f2 -> Difference f2 f1
    Product1d3d' f1 f2 -> Product1d3d' -f1 f2
    Product3d1d' f1 f2 -> Product3d1d' f1 -f2
    _ -> Negated function

instance Multiplication Sign (Function (space @ units)) (Function (space @ units))

instance Multiplication' Sign (Function (space @ units)) where
  type Sign .*. Function (space @ units) = Function (space @ (Unitless :*: units))
  Positive .*. function = Units.coerce function
  Negative .*. function = Units.coerce -function

instance Multiplication (Function (space @ units)) Sign (Function (space @ units))

instance Multiplication' (Function (space @ units)) Sign where
  type Function (space @ units) .*. Sign = Function (space @ (units :*: Unitless))
  function .*. Positive = Units.coerce function
  function .*. Negative = Units.coerce -function

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Addition
    (Function (space @ units))
    (Function (space_ @ units_))
    (Function (space @ units))
  where
  -- TODO add special cases
  f1 + f2 = Sum f1 f2

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Addition
    (Function (space @ units))
    (Vector3d (space_ @ units_))
    (Function (space @ units))
  where
  f + v = f + constant v

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Addition
    (Vector3d (space @ units))
    (Function (space_ @ units_))
    (Function (space @ units))
  where
  v + f = constant v + f

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Subtraction
    (Function (space @ units))
    (Function (space_ @ units_))
    (Function (space @ units))
  where
  -- TODO add special cases
  f1 - f2 = Difference f1 f2

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Subtraction
    (Function (space @ units))
    (Vector3d (space_ @ units_))
    (Function (space @ units))
  where
  f - v = f - constant v

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Subtraction
    (Vector3d (space @ units))
    (Function (space_ @ units_))
    (Function (space @ units))
  where
  v - f = constant v - f

wrap :: Interface function (space @ units) => function -> Function (space @ units)
wrap = Function

zero :: Function (space @ units)
zero = constant Vector3d.zero

constant :: Vector3d (space @ units) -> Function (space @ units)
constant = Constant

xyz ::
  Surface1d.Function units ->
  Surface1d.Function units ->
  Surface1d.Function units ->
  Function (space @ units)
xyz = XYZ

evaluate :: Function (space @ units) -> Uv.Point -> Vector3d (space @ units)
evaluate function uv = case function of
  Function f -> evaluateImpl f uv
  Coerce f -> Units.coerce (evaluate f uv)
  Constant v -> v
  XYZ x y z ->
    Vector3d.xyz
      (Surface1d.Function.evaluate x uv)
      (Surface1d.Function.evaluate y uv)
      (Surface1d.Function.evaluate z uv)
  Negated f -> negate (evaluate f uv)
  Sum f1 f2 -> evaluate f1 uv + evaluate f2 uv
  Difference f1 f2 -> evaluate f1 uv - evaluate f2 uv
  Product1d3d' f1 f2 -> Surface1d.Function.evaluate f1 uv .*. evaluate f2 uv
  Product3d1d' f1 f2 -> evaluate f1 uv .*. Surface1d.Function.evaluate f2 uv
  Quotient' f1 f2 -> evaluate f1 uv ./. Surface1d.Function.evaluate f2 uv

bounds :: Function (space @ units) -> Uv.Bounds -> VectorBounds3d (space @ units)
bounds function uv = case function of
  Function f -> boundsImpl f uv
  Coerce f -> Units.coerce (bounds f uv)
  Constant v -> VectorBounds3d.constant v
  XYZ x y z ->
    VectorBounds3d.xyz
      (Surface1d.Function.bounds x uv)
      (Surface1d.Function.bounds y uv)
      (Surface1d.Function.bounds z uv)
  Negated f -> negate (bounds f uv)
  Sum f1 f2 -> bounds f1 uv + bounds f2 uv
  Difference f1 f2 -> bounds f1 uv - bounds f2 uv
  Product1d3d' f1 f2 -> Surface1d.Function.bounds f1 uv .*. bounds f2 uv
  Product3d1d' f1 f2 -> bounds f1 uv .*. Surface1d.Function.bounds f2 uv
  Quotient' f1 f2 -> bounds f1 uv ./. Surface1d.Function.bounds f2 uv
