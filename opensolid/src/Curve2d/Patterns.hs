{-# LANGUAGE NoFieldSelectors #-}

module Curve2d.Patterns where

import Angle qualified
import {-# SOURCE #-} Curve2d (Curve2d)
import Curve2d.Internal qualified as Internal
import Direction2d (Direction2d)
import Maybe qualified
import OpenSolid
import Point2d (Point2d)
import Vector2d qualified

pattern Line :: Point2d (space @ units) -> Point2d (space @ units) -> Curve2d (space @ units)
pattern Line{startPoint, endPoint} <- Internal.Line startPoint endPoint

pattern Arc ::
  Tolerance units =>
  Point2d (space @ units) ->
  Direction2d space ->
  Direction2d space ->
  Qty units ->
  Qty units ->
  Angle ->
  Angle ->
  Curve2d (space @ units)
pattern Arc{centerPoint, majorDirection, minorDirection, majorRadius, minorRadius, startAngle, endAngle} <-
  ( \case
      Internal.Arc centerPoint vx vy a b -> Maybe.do
        let theta =
              if Vector2d.magnitude vx ~= Vector2d.magnitude vy
                then Angle.degrees 45.0
                else 0.5 * Angle.atan2 (2.0 * vx .<>. vy) (vx .<>. vx - vy .<>. vy)
        let cosTheta = Angle.cos theta
        let sinTheta = Angle.sin theta
        let v1 = vx * cosTheta + vy * sinTheta
        let v2 = vy * cosTheta - vx * sinTheta
        (r1, d1) <- Vector2d.magnitudeAndDirection v1 ?? Nothing
        (r2, d2) <- Vector2d.magnitudeAndDirection v2 ?? Nothing
        if r1 >= r2
          then do
            let startAngle = a - theta
            let endAngle = b - theta
            Just (centerPoint, d1, d2, r1, r2, startAngle, endAngle)
          else do
            let startAngle = a - theta - Angle.quarterTurn
            let endAngle = b - theta - Angle.quarterTurn
            Just (centerPoint, d2, -d1, r2, r1, startAngle, endAngle)
      _ -> Nothing ->
      Just (centerPoint, majorDirection, minorDirection, majorRadius, minorRadius, startAngle, endAngle)
    )
