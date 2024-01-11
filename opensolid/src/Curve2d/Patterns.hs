{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Curve2d.Patterns where

import {-# SOURCE #-} Curve2d (Curve2d)
import Curve2d.Internal qualified as Internal
import Direction2d (Direction2d)
import OpenSolid
import Point2d (Point2d)

pattern Line ::
  Point2d (space @ units) ->
  Point2d (space @ units) ->
  Direction2d space ->
  Qty units ->
  Curve2d (space @ units)
pattern Line {startPoint, endPoint, direction, length} <-
  Internal.Line {startPoint, endPoint, direction, length}

pattern Arc ::
  Point2d (space @ units) ->
  Qty units ->
  Angle ->
  Angle ->
  Curve2d (space @ units)
pattern Arc {centerPoint, radius, startAngle, endAngle} <-
  Internal.Arc {centerPoint, radius, startAngle, endAngle}
