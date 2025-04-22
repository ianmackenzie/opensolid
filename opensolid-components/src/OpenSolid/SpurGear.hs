module OpenSolid.SpurGear
  ( SpurGear
  , metric
  , module_
  , numTeeth
  , profile
  , pitchDiameter
  , outerDiameter
  )
where

import OpenSolid.Angle qualified as Angle
import OpenSolid.Axis2d qualified as Axis2d
import OpenSolid.Curve qualified as Curve
import OpenSolid.Curve2d (Curve2d)
import OpenSolid.Curve2d qualified as Curve2d
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Float qualified as Float
import OpenSolid.Length (Length)
import OpenSolid.List qualified as List
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Prelude
import OpenSolid.Units (Meters)
import OpenSolid.Vector2d qualified as Vector2d
import OpenSolid.VectorCurve2d qualified as VectorCurve2d

-- | A metric spur gear.
data SpurGear = Metric
  { numTeeth :: Int
  -- ^ Get the number of teeth of a gear.
  , module_ :: Length
  -- ^ Get the module of a gear.
  }

instance FFI SpurGear where
  representation = FFI.classRepresentation "SpurGear"

-- | Create a metric spur gear with the given number of teeth and module.
metric :: Named "numTeeth" Int -> Named "module" Length -> SpurGear
metric (Named numTeeth) (Named module_) = Metric{numTeeth, module_}

-- | Get the pitch diameter of a gear.
pitchDiameter :: SpurGear -> Length
pitchDiameter gear = module_ gear * Float.int (numTeeth gear)

{-| Get the outer diameter of a gear.

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
profile :: Tolerance Meters => SpurGear -> List (Curve2d (space @ Meters))
profile gear = do
  let n = numTeeth gear
  let m = module_ gear
  let phi = Angle.degrees 20.0 -- pressure angle
  let r0 = m * Float.int n / 2.0 -- pitch radius
  let rb = r0 * Angle.cos phi -- involute tooth profile base radius
  let rd = r0 - 1.25 * m -- dedendum radius
  let ra = r0 + m -- addendum radius
  let theta1
        | rd > rb = Angle.radians (Float.sqrt (Float.squared (rd / rb) - 1.0))
        | otherwise = Angle.zero
  let theta2 = Angle.radians (Float.sqrt (Float.squared (ra / rb) - 1.0))
  let theta = Curve.line theta1 theta2
  let alpha = Angle.radians (Angle.tan phi - Angle.inRadians phi + Float.pi / (2.0 * Float.int n))
  let x = rb * (Curve.sin (theta - alpha) - theta / Angle.radian * Curve.cos (theta - alpha))
  let y = rb * (Curve.cos (theta - alpha) + theta / Angle.radian * Curve.sin (theta - alpha))
  let involuteLeft = Curve2d.xy x y
  let leftStart = Curve2d.startPoint involuteLeft
  let leftEnd = Curve2d.endPoint involuteLeft
  let involuteLeftDerivative = Curve2d.derivative involuteLeft
  let leftStartTangent
        | rd > rb = Vector2d.normalize (VectorCurve2d.startValue involuteLeftDerivative)
        | otherwise = Vector2d.polar 1.0 (Angle.quarterTurn + alpha)
  let leftEndTangent = Vector2d.normalize (VectorCurve2d.endValue involuteLeftDerivative)
  let leftDerivativeMagnitude = Point2d.distanceFrom leftStart leftEnd
  let leftApproximation =
        Curve2d.hermite
          leftStart
          [leftDerivativeMagnitude * leftStartTangent]
          leftEnd
          [leftDerivativeMagnitude * leftEndTangent]
  let rightApproximation = Curve2d.mirrorAcross Axis2d.y leftApproximation
  let tip = Curve2d.line (Curve2d.endPoint leftApproximation) (Curve2d.endPoint rightApproximation)
  let angularSpacing = Angle.twoPi / Float.int n
  let nextToothStart =
        Curve2d.startPoint rightApproximation
          |> Point2d.rotateAround Point2d.origin angularSpacing
  let connector
        | rd > rb = Curve2d.line leftStart nextToothStart
        | otherwise = Curve2d.arc leftStart nextToothStart -Angle.pi
  let toothProfile = [leftApproximation, rightApproximation, tip, connector]
  let rotatedProfile i = do
        let angle = Float.int i * angularSpacing
        List.map (Curve2d.rotateAround Point2d.origin angle) toothProfile
  List.collect rotatedProfile [0 .. n - 1]
