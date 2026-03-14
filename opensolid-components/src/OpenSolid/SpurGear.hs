module OpenSolid.SpurGear
  ( SpurGear
  , numTeeth
  , module_
  , pitchDiameter
  , outerDiameter
  , metric
  , profile
  )
where

import OpenSolid.Angle qualified as Angle
import OpenSolid.Axis2D qualified as Axis2D
import OpenSolid.Curve1D qualified as Curve1D
import OpenSolid.Curve2D (Curve2D)
import OpenSolid.Curve2D qualified as Curve2D
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Length (Length)
import OpenSolid.List qualified as List
import OpenSolid.Number qualified as Number
import OpenSolid.Point2D qualified as Point2D
import OpenSolid.Prelude
import OpenSolid.Vector2D qualified as Vector2D

-- | A metric spur gear.
newtype SpurGear = Metric ("numTeeth" ::: Int, "module_" ::: Length)

instance FFI SpurGear where
  representation = FFI.classRepresentation "SpurGear"

-- | Create a metric spur gear with the given number of teeth and module.
metric :: "numTeeth" ::: Int -> "module" ::: Length -> SpurGear
metric ("numTeeth" ::: givenNumTeeth) ("module" ::: givenModule) =
  Metric (#numTeeth givenNumTeeth, #module_ givenModule)

-- | The number of teeth of a gear.
numTeeth :: SpurGear -> Int
numTeeth (Metric fields) = fields.numTeeth

-- | The module of a gear.
module_ :: SpurGear -> Length
module_ (Metric fields) = fields.module_

-- | The pitch diameter of a gear.
pitchDiameter :: SpurGear -> Length
pitchDiameter gear = module_ gear * Number.fromInt (numTeeth gear)

{-| The outer diameter of a gear.

This is equal to the pitch diameter plus twice the module.
-}
outerDiameter :: SpurGear -> Length
outerDiameter gear = pitchDiameter gear + 2.0 * module_ gear

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
profile :: Tolerance Meters => SpurGear -> List (Curve2D Meters space)
profile gear = do
  let n = numTeeth gear
  let m = module_ gear
  let phi = Angle.degrees 20.0 -- pressure angle
  let r0 = m * Number.fromInt n / 2.0 -- pitch radius
  let rb = r0 * Angle.cos phi -- involute tooth profile base radius
  let rd = r0 - 1.25 * m -- dedendum radius
  let ra = r0 + m -- addendum radius
  let theta1
        | rd > rb = Angle.radians (Number.sqrt (Number.squared (rd / rb) - 1.0))
        | otherwise = Angle.zero
  let theta2 = Angle.radians (Number.sqrt (Number.squared (ra / rb) - 1.0))
  let theta = Curve1D.interpolateFrom theta1 theta2
  let alpha = Angle.radians (Angle.tan phi - Angle.inRadians phi + Number.pi / Number.fromInt (2 * n))
  let x = rb * (Curve1D.sin (theta - alpha) - theta / Angle.radian * Curve1D.cos (theta - alpha))
  let y = rb * (Curve1D.cos (theta - alpha) + theta / Angle.radian * Curve1D.sin (theta - alpha))
  let involuteLeft = Curve2D.xy x y
  let leftStart = involuteLeft.startPoint
  let leftEnd = involuteLeft.endPoint
  let leftStartTangent
        | rd > rb = Vector2D.normalize (Curve2D.derivativeValue involuteLeft 0.0)
        | otherwise = Vector2D.polar 1.0 (Angle.halfPi + alpha)
  let leftEndTangent = Vector2D.normalize (Curve2D.derivativeValue involuteLeft 1.0)
  let leftDerivativeMagnitude = Point2D.distanceFrom leftStart leftEnd
  let leftApproximation =
        Curve2D.hermite
          leftStart
          [leftDerivativeMagnitude * leftStartTangent]
          leftEnd
          [leftDerivativeMagnitude * leftEndTangent]
  let rightApproximation = Curve2D.mirrorAcross Axis2D.y leftApproximation
  let tip = Curve2D.lineFrom leftApproximation.endPoint rightApproximation.endPoint
  let angularSpacing = Angle.twoPi / Number.fromInt n
  let nextToothStart =
        Point2D.rotateAround Point2D.origin angularSpacing rightApproximation.startPoint
  let connector
        | rd > rb = Curve2D.lineFrom leftStart nextToothStart
        | otherwise = Curve2D.arcFrom leftStart nextToothStart -Angle.pi
  let toothProfileCurves = [leftApproximation, rightApproximation, tip, connector]
  let rotatedProfileCurves i = do
        let angle = Number.fromInt i * angularSpacing
        List.map (Curve2D.rotateAround Point2D.origin angle) toothProfileCurves
  List.combine rotatedProfileCurves [0 .. n - 1]
