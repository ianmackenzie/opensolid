module OpenSolid.Curve2d
  ( Curve2d (Parametric, Transformed)
  , Interface (..)
  , HasDegeneracy (HasDegeneracy)
  , new
  , startPoint
  , endPoint
  , evaluate
  , evaluateBounds
  , bounds
  , derivative
  , tangentDirection
  , reverse
  , removeStartDegeneracy
  , transformBy
  , piecewise
  , unsafePiecewise
  )
where

import OpenSolid.Bounds2d (Bounds2d)
import OpenSolid.Curve (Curve)
import OpenSolid.DirectionCurve2d (DirectionCurve2d)
import OpenSolid.Expression (Expression)
import OpenSolid.Frame2d (Frame2d)
import OpenSolid.Point2d (Point2d)
import OpenSolid.Prelude
import OpenSolid.Range (Range)
import OpenSolid.Range qualified as Range
import OpenSolid.Transform2d (Transform2d)
import OpenSolid.Vector2d (Vector2d)
import OpenSolid.VectorCurve2d (VectorCurve2d)

type role Curve2d nominal

data Curve2d (coordinateSystem :: CoordinateSystem) where
  Curve ::
    Interface curve coordinateSystem =>
    curve ->
    Curve2d coordinateSystem
  Parametric ::
    Expression Float (Point2d (space @ units)) ->
    Curve2d (space @ units)
  Coerce ::
    Curve2d (space @ units1) ->
    Curve2d (space @ units2)
  XY ::
    Curve units ->
    Curve units ->
    Curve2d (space @ units)
  PlaceIn ::
    Frame2d (global @ units) (Defines local) ->
    Curve2d (local @ units) ->
    Curve2d (global @ units)
  Transformed ::
    Transform2d tag (space @ units) ->
    Curve2d (space @ units) ->
    Curve2d (space @ units)

instance Show (Curve2d (space @ units))

class
  Show curve =>
  Interface curve (coordinateSystem :: CoordinateSystem)
    | curve -> coordinateSystem
  where
  startPointImpl :: curve -> Point2d coordinateSystem
  endPointImpl :: curve -> Point2d coordinateSystem
  evaluateImpl :: curve -> Float -> Point2d coordinateSystem
  evaluateBoundsImpl :: curve -> Range Unitless -> Bounds2d coordinateSystem
  derivativeImpl :: curve -> VectorCurve2d coordinateSystem
  reverseImpl :: curve -> Curve2d coordinateSystem
  boundsImpl :: curve -> Bounds2d coordinateSystem
  transformByImpl :: Transform2d tag coordinateSystem -> curve -> Curve2d coordinateSystem

  startPointImpl curve = evaluateImpl curve 0.0
  endPointImpl curve = evaluateImpl curve 1.0
  boundsImpl curve = evaluateBoundsImpl curve Range.unit

data HasDegeneracy = HasDegeneracy

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Addition
    (Curve2d (space1 @ units1))
    (VectorCurve2d (space2 @ units2))
    (Curve2d (space1 @ units1))

new :: Interface curve (space @ units) => curve -> Curve2d (space @ units)
startPoint :: Curve2d (space @ units) -> Point2d (space @ units)
endPoint :: Curve2d (space @ units) -> Point2d (space @ units)
evaluate :: Curve2d (space @ units) -> Float -> Point2d (space @ units)
evaluateBounds :: Curve2d (space @ units) -> Range Unitless -> Bounds2d (space @ units)
bounds :: Curve2d (space @ units) -> Bounds2d (space @ units)
derivative :: Curve2d (space @ units) -> VectorCurve2d (space @ units)
tangentDirection ::
  Tolerance units =>
  Curve2d (space @ units) ->
  Result HasDegeneracy (DirectionCurve2d space)
reverse :: Curve2d (space @ units) -> Curve2d (space @ units)
removeStartDegeneracy ::
  Int ->
  (Point2d (space @ units), List (Vector2d (space @ units))) ->
  Curve2d (space @ units) ->
  Curve2d (space @ units)
transformBy ::
  Transform2d tag (space @ units) ->
  Curve2d (space @ units) ->
  Curve2d (space @ units)
piecewise ::
  Tolerance units =>
  NonEmpty (Curve2d (space @ units)) ->
  Result HasDegeneracy (Curve2d (space @ units))
unsafePiecewise :: NonEmpty (Curve2d (space @ units)) -> Curve2d (space @ units)
