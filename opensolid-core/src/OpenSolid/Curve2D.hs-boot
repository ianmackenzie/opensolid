{-# OPTIONS_GHC -Wno-orphans #-}

module OpenSolid.Curve2D
  ( Curve2D
  , Compiled
  , SearchTree
  , constant
  , new
  , startPoint
  , endPoint
  , point
  , bounds
  , overallBounds
  , compiled
  , derivative
  , reverse
  , xy
  , lineFrom
  , hermite
  , desingularize
  , transformBy
  , piecewise
  , searchTree
  )
where

import {-# SOURCE #-} OpenSolid.CompiledFunction (CompiledFunction)
import {-# SOURCE #-} OpenSolid.Curve (Curve)
import {-# SOURCE #-} OpenSolid.Curve.Search qualified as Curve.Search
import {-# SOURCE #-} OpenSolid.Curve1D (Curve1D)
import OpenSolid.Interval (Interval)
import OpenSolid.Prelude
import OpenSolid.Primitives (Bounds2D, Point2D, Transform2D, Vector2D)
import {-# SOURCE #-} OpenSolid.SurfaceFunction1D (SurfaceFunction1D)
import OpenSolid.UvSpace (UvSpace)
import {-# SOURCE #-} OpenSolid.VectorCurve2D (VectorCurve2D)

type Curve2D units space = Curve 2 units space

type Compiled units space =
  CompiledFunction
    Number
    (Point2D units space)
    (Interval Unitless)
    (Bounds2D units space)

type SearchTree units space = Curve.Search.Tree 2 units space

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Curve2D units1 space1)
    (VectorCurve2D units2 space2)
    (Curve2D units1 space1)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Curve2D units1 space1)
    (Vector2D units2 space2)
    (Curve2D units1 space1)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Curve2D units1 space1)
    (VectorCurve2D units2 space2)
    (Curve2D units1 space1)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Curve2D units1 space1)
    (Vector2D units2 space2)
    (Curve2D units1 space1)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Curve2D units1 space1)
    (Curve2D units2 space2)
    (VectorCurve2D units1 space1)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Curve2D units1 space1)
    (Point2D units2 space2)
    (VectorCurve2D units1 space1)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Point2D units1 space1)
    (Curve2D units2 space2)
    (VectorCurve2D units1 space1)

instance
  (uvSpace ~ UvSpace, unitless ~ Unitless) =>
  Composition
    (SurfaceFunction1D units)
    (Curve2D unitless uvSpace)
    (Curve1D units)

constant :: Point2D units space -> Curve2D units space
new :: Compiled units space -> VectorCurve2D units space -> Curve2D units space
startPoint :: Curve2D units space -> Point2D units space
endPoint :: Curve2D units space -> Point2D units space
point :: Curve2D units space -> Number -> Point2D units space
bounds :: Curve2D units space -> Interval Unitless -> Bounds2D units space
overallBounds :: Curve2D units space -> Bounds2D units space
compiled :: Curve2D units space -> Compiled units space
derivative :: Curve2D units space -> VectorCurve2D units space
reverse :: Curve2D units space -> Curve2D units space
xy :: Curve1D units -> Curve1D units -> Curve2D units space
lineFrom :: Point2D units space -> Point2D units space -> Curve2D units space
hermite ::
  Point2D units space ->
  List (Vector2D units space) ->
  Point2D units space ->
  List (Vector2D units space) ->
  Curve2D units space
desingularize ::
  Maybe (Point2D units space, Vector2D units space) ->
  Curve2D units space ->
  Maybe (Point2D units space, Vector2D units space) ->
  Curve2D units space
transformBy ::
  Transform2D tag units space ->
  Curve2D units space ->
  Curve2D units space
piecewise ::
  Tolerance units =>
  NonEmpty (Curve2D units space) ->
  Curve2D units space
searchTree :: Curve2D units space -> SearchTree units space
