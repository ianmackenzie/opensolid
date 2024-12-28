module OpenSolid.Curve2d.Function
  ( Function (Parametric)
  , Interface (..)
  , new
  , evaluate
  , evaluateBounds
  , derivative
  , reverse
  , transformBy
  , TransformBy (TransformBy)
  )
where

import OpenSolid.Bounds2d (Bounds2d)
import OpenSolid.Expression (Expression)
import OpenSolid.Frame2d (Frame2d)
import OpenSolid.Point2d (Point2d)
import OpenSolid.Prelude
import OpenSolid.Range (Range)
import OpenSolid.Transform2d (Transform2d)
import OpenSolid.VectorCurve2d.Function qualified as VectorCurve2d.Function

type role Function nominal

data Function (coordinateSystem :: CoordinateSystem) where
  Function ::
    Interface function coordinateSystem =>
    function ->
    Function coordinateSystem
  Parametric ::
    Expression Float (Point2d (space @ units)) ->
    Function (space @ units)
  Coerce ::
    Function (space @ units1) ->
    Function (space @ units2)
  PlaceIn ::
    Frame2d (global @ units) (Defines local) ->
    Function (local @ units) ->
    Function (global @ units)
  Addition ::
    Function (space @ units) ->
    VectorCurve2d.Function.Function (space @ units) ->
    Function (space @ units)
  Subtraction ::
    Function (space @ units) ->
    VectorCurve2d.Function.Function (space @ units) ->
    Function (space @ units)

instance Show (Function (space @ units))

class
  Show function =>
  Interface function (coordinateSystem :: CoordinateSystem)
    | function -> coordinateSystem
  where
  evaluateImpl :: function -> Float -> Point2d coordinateSystem
  evaluateBoundsImpl :: function -> Range Unitless -> Bounds2d coordinateSystem
  derivativeImpl :: function -> VectorCurve2d.Function.Function coordinateSystem
  reverseImpl :: function -> function
  transformByImpl :: Transform2d tag coordinateSystem -> function -> Function coordinateSystem

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Function (space1 @ units1))
    (VectorCurve2d.Function.Function (space2 @ units2))
    (Function (space1 @ units1))

new :: Interface function (space @ units) => function -> Function (space @ units)
evaluate :: Function (space @ units) -> Float -> Point2d (space @ units)
evaluateBounds :: Function (space @ units) -> Range Unitless -> Bounds2d (space @ units)
derivative :: Function (space @ units) -> VectorCurve2d.Function.Function (space @ units)
reverse :: Function (space @ units) -> Function (space @ units)
transformBy ::
  Transform2d tag (space @ units) ->
  Function (space @ units) ->
  Function (space @ units)

data TransformBy function coordinateSystem where
  TransformBy ::
    Interface function (space @ units) =>
    Transform2d tag (space @ units) ->
    function ->
    TransformBy function (space @ units)

instance Show (TransformBy function (space @ units))

instance Interface (TransformBy function (space @ units)) (space @ units)
