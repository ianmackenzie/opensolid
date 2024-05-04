{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Curve2d.Patterns where

import {-# SOURCE #-} Curve2d (Curve2d)
import Curve2d.Internal qualified as Internal
import OpenSolid
import Point2d (Point2d)
import Vector2d (Vector2d)

pattern Line ::
  Point2d (space @ units) ->
  Point2d (space @ units) ->
  Curve2d (space @ units)
pattern Line{startPoint, endPoint} <-
  Internal.Line{startPoint, endPoint}

pattern Arc ::
  Point2d (space @ units) ->
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  Angle ->
  Angle ->
  Curve2d (space @ units)
pattern Arc{centerPoint, xVector, yVector, startAngle, endAngle} <-
  Internal.Arc{centerPoint, xVector, yVector, startAngle, endAngle}
