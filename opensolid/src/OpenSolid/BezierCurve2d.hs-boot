module OpenSolid.BezierCurve2d (hermite) where

import OpenSolid.Prelude
import {-# SOURCE #-} OpenSolid.Curve2d (Curve2d)
import OpenSolid.Point2d (Point2d)
import OpenSolid.Vector2d (Vector2d)

hermite ::
  (Point2d (space @ units), List (Vector2d (space @ units))) ->
  (Point2d (space @ units), List (Vector2d (space @ units))) ->
  Curve2d (space @ units)
