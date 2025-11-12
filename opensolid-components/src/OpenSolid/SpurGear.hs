module OpenSolid.SpurGear
  ( SpurGear (numTeeth, module_)
  , metric
  , profile
  )
where

import GHC.Records (HasField (getField))
import OpenSolid.Angle qualified as Angle
import OpenSolid.Axis2d qualified as Axis2d
import OpenSolid.Curve qualified as Curve
import OpenSolid.Curve2d (Curve2d)
import OpenSolid.Curve2d qualified as Curve2d
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Length (Length)
import OpenSolid.List qualified as List
import OpenSolid.Number qualified as Number
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Prelude
import OpenSolid.Vector2d qualified as Vector2d
import OpenSolid.VectorCurve2d qualified as VectorCurve2d

-- | A metric spur gear.
data SpurGear = Metric
  { numTeeth :: Int
  -- ^ The number of teeth of a gear.
  , module_ :: Length
  -- ^ The module of a gear.
  }

instance FFI SpurGear where
  representation = FFI.classRepresentation "SpurGear"

-- | Create a metric spur gear with the given number of teeth and module.
metric :: "numTeeth" ::: Int -> "module" ::: Length -> SpurGear
metric (Named numTeeth) (Named module_) = Metric{numTeeth, module_}

-- | The pitch diameter of a gear.
instance HasField "pitchDiameter" SpurGear Length where
  getField gear = gear.module_ .*. Number.fromInt gear.numTeeth

{-| The outer diameter of a gear.

This is equal to the pitch diameter plus twice the module.
-}
instance HasField "outerDiameter" SpurGear Length where
  getField gear = gear.pitchDiameter .+. 2 *. gear.module_

{-| Get the outer profile of a gear as a list of curves, centered at the origin.

This is just the profile of the gear teeth themselves,
and does not include a bore hole or anything else
(lightening holes etc.).
It is expected that you will combine this with
any additional curves you want
(likely at least one circle for a bore hole)
and then construct a profile region from the combined set of curves
that you can then extrude to form a gear body.
-}
profile :: Tolerance Meters => SpurGear -> List (Curve2d (space @ Meters))
profile gear = do
  let n = gear.numTeeth
  let m = gear.module_
  let phi = Angle.degrees 20 -- pressure angle
  let r0 = m .*. Number.fromInt n ./ 2 -- pitch radius
  let rb = r0 .*. Angle.cos phi -- involute tooth profile base radius
  let rd = r0 .-. 1.25 *. m -- dedendum radius
  let ra = r0 .+. m -- addendum radius
  let theta1
        | rd > rb = Angle.radians (Number.sqrt (Number.squared (rd ./. rb) .- 1))
        | otherwise = Angle.zero
  let theta2 = Angle.radians (Number.sqrt (Number.squared (ra ./. rb) .- 1))
  let theta = Curve.line theta1 theta2
  let alpha = Angle.radians (Angle.tan phi .-. Angle.inRadians phi .+. Number.pi ./. Number.fromInt (2 * n))
  let x = rb .*. (Curve.sin (theta .-. alpha) .-. theta ./. Angle.radian .*. Curve.cos (theta .-. alpha))
  let y = rb .*. (Curve.cos (theta .-. alpha) .+. theta ./. Angle.radian .*. Curve.sin (theta .-. alpha))
  let involuteLeft = Curve2d.xy x y
  let leftStart = involuteLeft.startPoint
  let leftEnd = involuteLeft.endPoint
  let involuteLeftDerivative = involuteLeft.derivative
  let leftStartTangent
        | rd > rb = Vector2d.normalize (VectorCurve2d.startValue involuteLeftDerivative)
        | otherwise = Vector2d.polar 1 (Angle.halfPi .+. alpha)
  let leftEndTangent = Vector2d.normalize (VectorCurve2d.endValue involuteLeftDerivative)
  let leftDerivativeMagnitude = Point2d.distanceFrom leftStart leftEnd
  let leftApproximation =
        Curve2d.hermite
          leftStart
          [leftDerivativeMagnitude .*. leftStartTangent]
          leftEnd
          [leftDerivativeMagnitude .*. leftEndTangent]
  let rightApproximation = Curve2d.mirrorAcross Axis2d.y leftApproximation
  let tip = Curve2d.line leftApproximation.endPoint rightApproximation.endPoint
  let angularSpacing = Angle.twoPi ./. Number.fromInt n
  let nextToothStart =
        Point2d.rotateAround Point2d.origin angularSpacing rightApproximation.startPoint
  let connector
        | rd > rb = Curve2d.line leftStart nextToothStart
        | otherwise = Curve2d.arc leftStart nextToothStart (negative Angle.pi)
  let toothProfileCurves = [leftApproximation, rightApproximation, tip, connector]
  let rotatedProfileCurves i = do
        let angle = Number.fromInt i .*. angularSpacing
        List.map (Curve2d.rotateAround Point2d.origin angle) toothProfileCurves
  List.combine rotatedProfileCurves [0 .. n - 1]
