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

import Data.Void (Void)
import {-# SOURCE #-} OpenSolid.Curve (Curve2D)
import {-# SOURCE #-} OpenSolid.Curve qualified as Curve
import {-# SOURCE #-} OpenSolid.Curve.Search qualified as Curve.Search
import {-# SOURCE #-} OpenSolid.Curve1D (Curve1D)
import OpenSolid.Interval (Interval)
import OpenSolid.Prelude
import OpenSolid.Primitives (Bounds2D, Point2D, Transform2D, Vector2D)
import {-# SOURCE #-} OpenSolid.VectorCurve2D (VectorCurve2D)

type Compiled units = Curve.Compiled 2 units Void

type SearchTree units = Curve.Search.Tree 2 units Void

constant :: Point2D units -> Curve2D units
new :: Compiled units -> VectorCurve2D units -> Curve2D units
startPoint :: Curve2D units -> Point2D units
endPoint :: Curve2D units -> Point2D units
point :: Curve2D units -> Number -> Point2D units
bounds :: Curve2D units -> Interval Unitless -> Bounds2D units
overallBounds :: Curve2D units -> Bounds2D units
compiled :: Curve2D units -> Compiled units
derivative :: Curve2D units -> VectorCurve2D units
reverse :: Curve2D units -> Curve2D units
xy :: Curve1D units -> Curve1D units -> Curve2D units
lineFrom :: Point2D units -> Point2D units -> Curve2D units
hermite ::
  Point2D units ->
  List (Vector2D units) ->
  Point2D units ->
  List (Vector2D units) ->
  Curve2D units
desingularize ::
  Maybe (Point2D units, Vector2D units) ->
  Curve2D units ->
  Maybe (Point2D units, Vector2D units) ->
  Curve2D units
transformBy :: Transform2D tag units -> Curve2D units -> Curve2D units
piecewise :: Tolerance units => NonEmpty (Curve2D units) -> Curve2D units
searchTree :: Curve2D units -> SearchTree units
