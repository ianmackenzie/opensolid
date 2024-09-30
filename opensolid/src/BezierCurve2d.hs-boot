module BezierCurve2d (hermite) where

import {-# SOURCE #-} Curve2d (Curve2d)
import OpenSolid
import Point2d (Point2d)
import Vector2d (Vector2d)

hermite ::
  (Point2d (space @ units), List (Vector2d (space @ units))) ->
  (Point2d (space @ units), List (Vector2d (space @ units))) ->
  Curve2d (space @ units)
