module OpenSolid.API (classes, functions) where

import OpenSolid.API.Class (Class, Self (Self))
import OpenSolid.API.Class qualified as Class
import OpenSolid.API.Docs (docs)
import OpenSolid.API.Function (Function)
import OpenSolid.API.Space (Space)
import OpenSolid.Angle (Angle)
import OpenSolid.Angle qualified as Angle
import OpenSolid.Area (Area)
import OpenSolid.Area qualified as Area
import OpenSolid.Axis2d (Axis2d (Axis2d))
import OpenSolid.Axis2d qualified as Axis2d
import OpenSolid.Axis3d (Axis3d)
import OpenSolid.Axis3d qualified as Axis3d
import OpenSolid.Body3d (Body3d)
import OpenSolid.Body3d qualified as Body3d
import OpenSolid.Bounds (Bounds (Bounds))
import OpenSolid.Bounds qualified as Bounds
import OpenSolid.Bounds2d (Bounds2d (Bounds2d))
import OpenSolid.Bounds2d qualified as Bounds2d
import OpenSolid.Bounds3d (Bounds3d)
import OpenSolid.Bounds3d qualified as Bounds3d
import OpenSolid.Camera3d (Camera3d)
import OpenSolid.Camera3d qualified as Camera3d
import OpenSolid.Color (Color)
import OpenSolid.Color qualified as Color
import OpenSolid.Convention3d (Convention3d)
import OpenSolid.Convention3d qualified as Convention3d
import OpenSolid.Curve (Curve)
import OpenSolid.Curve qualified as Curve
import OpenSolid.Curve.Zero qualified as Curve.Zero
import OpenSolid.Curve2d (Curve2d)
import OpenSolid.Curve2d qualified as Curve2d
import OpenSolid.Direction2d (Direction2d)
import OpenSolid.Direction2d qualified as Direction2d
import OpenSolid.Direction3d (Direction3d)
import OpenSolid.Direction3d qualified as Direction3d
import OpenSolid.Drawing2d (Drawing2d)
import OpenSolid.Drawing2d qualified as Drawing2d
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Frame3d (Frame3d)
import OpenSolid.Frame3d qualified as Frame3d
import OpenSolid.Gltf qualified as Gltf
import OpenSolid.Length (Length)
import OpenSolid.Length qualified as Length
import OpenSolid.List qualified as List
import OpenSolid.Mitsuba qualified as Mitsuba
import OpenSolid.Model3d (Model3d)
import OpenSolid.Model3d qualified as Model3d
import OpenSolid.Orientation3d (Orientation3d)
import OpenSolid.Orientation3d qualified as Orientation3d
import OpenSolid.PbrMaterial (PbrMaterial)
import OpenSolid.PbrMaterial qualified as PbrMaterial
import OpenSolid.Plane3d (Plane3d)
import OpenSolid.Plane3d qualified as Plane3d
import OpenSolid.PlaneOrientation3d (PlaneOrientation3d)
import OpenSolid.PlaneOrientation3d qualified as PlaneOrientation3d
import OpenSolid.Point2d (Point2d (Point2d))
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Point3d (Point3d)
import OpenSolid.Point3d qualified as Point3d
import OpenSolid.Prelude
import OpenSolid.Qty qualified as Qty
import OpenSolid.Region2d (Region2d)
import OpenSolid.Region2d qualified as Region2d
import OpenSolid.Resolution (Resolution (Resolution))
import OpenSolid.Resolution qualified as Resolution
import OpenSolid.SpurGear (SpurGear)
import OpenSolid.SpurGear qualified as SpurGear
import OpenSolid.Stl qualified as Stl
import OpenSolid.Text qualified as Text
import OpenSolid.Transform qualified as Transform
import OpenSolid.Transform2d (Transform2d)
import OpenSolid.Transform2d qualified as Transform2d
import OpenSolid.Transform3d (Transform3d)
import OpenSolid.Transform3d qualified as Transform3d
import OpenSolid.Vector2d (Vector2d (Vector2d))
import OpenSolid.Vector2d qualified as Vector2d
import OpenSolid.Vector3d (Vector3d)
import OpenSolid.Vector3d qualified as Vector3d
import OpenSolid.VectorCurve2d (VectorCurve2d)
import OpenSolid.VectorCurve2d qualified as VectorCurve2d
import Prelude (flip)

classes :: List Class
classes =
  [ length
  , area
  , angle
  , bounds
  , lengthBounds
  , areaBounds
  , angleBounds
  , color
  , vector2d
  , displacement2d
  , areaVector2d
  , direction2d
  , point2d
  , uvPoint
  , bounds2d
  , uvBounds
  , curve
  , lengthCurve
  , areaCurve
  , angleCurve
  , drawing2d
  , axis2d
  , uvAxis
  , convention3d
  , vector3d
  , displacement3d
  , areaVector3d
  , direction3d
  , point3d
  , bounds3d
  , axis3d
  , planeOrientation3d
  , plane3d
  , orientation3d
  , frame3d
  , vectorCurve2d
  , displacementCurve2d
  , curve2d
  , uvCurve
  , region2d
  , uvRegion
  , body3d
  , resolution
  , pbrMaterial
  , model3d
  , gltf
  , spurGear
  , camera3d
  , mitsuba
  ]

functions :: List Function
functions = List.collect Class.functions classes

length :: Class
length =
  Class.new @Length $(docs ''Length) $
    [ Class.constant "Zero" Length.zero $(docs 'Length.zero)
    , Class.factory3 "Interpolate" "Start" "End" "Parameter Value" Qty.interpolateFrom $(docs 'Qty.interpolateFrom)
    , Class.static3 "Steps" "Start" "End" "N" (Qty.steps @Meters) $(docs 'Qty.steps)
    , Class.static3 "Leading" "Start" "End" "N" (Qty.leading @Meters) $(docs 'Qty.leading)
    , Class.static3 "Trailing" "Start" "End" "N" (Qty.trailing @Meters) $(docs 'Qty.trailing)
    , Class.static3 "In Between" "Start" "End" "N" (Qty.inBetween @Meters) $(docs 'Qty.inBetween)
    , Class.static3 "Midpoints" "Start" "End" "N" (Qty.midpoints @Meters) $(docs 'Qty.midpoints)
    , Class.factory1 "Meters" "Value" Length.meters $(docs 'Length.meters)
    , Class.factory1 "Centimeters" "Value" Length.centimeters $(docs 'Length.centimeters)
    , Class.factory1 "Cm" "Value" Length.cm $(docs 'Length.cm)
    , Class.factory1 "Millimeters" "Value" Length.millimeters $(docs 'Length.millimeters)
    , Class.factory1 "Mm" "Value" Length.mm $(docs 'Length.mm)
    , Class.factory1 "Micrometers" "Value" Length.micrometers $(docs 'Length.micrometers)
    , Class.factory1 "Nanometers" "Value" Length.nanometers $(docs 'Length.nanometers)
    , Class.factory1 "Inches" "Value" Length.inches $(docs 'Length.inches)
    , Class.factory1 "Pixels" "Value" Length.pixels $(docs 'Length.pixels)
    , Class.member0 "In Meters" Length.inMeters $(docs 'Length.inMeters)
    , Class.member0 "In Centimeters" Length.inCentimeters $(docs 'Length.inCentimeters)
    , Class.member0 "In Millimeters" Length.inMillimeters $(docs 'Length.inMillimeters)
    , Class.member0 "In Micrometers" Length.inMicrometers $(docs 'Length.inMicrometers)
    , Class.member0 "In Nanometers" Length.inNanometers $(docs 'Length.inNanometers)
    , Class.member0 "In Inches" Length.inInches $(docs 'Length.inInches)
    , Class.member0 "In Pixels" Length.inPixels $(docs 'Length.inPixels)
    , Class.memberM0 "Is Zero" (~= Length.zero) "Check if a length is zero, within the current tolerance."
    , Class.equalityAndHash
    , Class.comparison
    , Class.negateSelf
    , Class.absSelf Qty.abs
    , Class.floatTimes
    , Class.plusSelf
    , Class.plus @(Bounds Meters) Self
    , Class.plus @(Curve Meters) Self
    , Class.minusSelf
    , Class.minus @(Bounds Meters) Self
    , Class.minus @(Curve Meters) Self
    , Class.timesFloat
    , Class.timesSelf
    , Class.times @(Bounds Unitless) Self
    , Class.times @(Bounds Meters) Self
    , Class.times @(Curve Unitless) Self
    , Class.times @(Curve Meters) Self
    , Class.times @(Direction2d Space) Self
    , Class.times @(Vector2d (Space @ Unitless)) Self
    , Class.times @(Vector2d (Space @ Meters)) Self
    , Class.divByFloat
    , Class.divBySelf
    , Class.divBy @(Bounds Unitless) Self
    , Class.divBy @(Bounds Meters) Self
    , Class.divByU (\val crv -> Curve.quotient (Curve.constant val) crv)
    , Class.divByM (\val crv -> Curve.quotient (Curve.constant val) crv)
    , Class.floorDivBySelf
    , Class.modBySelf
    ]

area :: Class
area =
  Class.new @Area $(docs ''Area) $
    [ Class.constant "Zero" Area.zero $(docs 'Area.zero)
    , Class.factory3 "Interpolate" "Start" "End" "Parameter Value" Qty.interpolateFrom $(docs 'Qty.interpolateFrom)
    , Class.static3 "Steps" "Start" "End" "N" (Qty.steps @SquareMeters) $(docs 'Qty.steps)
    , Class.static3 "Leading" "Start" "End" "N" (Qty.leading @SquareMeters) $(docs 'Qty.leading)
    , Class.static3 "Trailing" "Start" "End" "N" (Qty.trailing @SquareMeters) $(docs 'Qty.trailing)
    , Class.static3 "In Between" "Start" "End" "N" (Qty.inBetween @SquareMeters) $(docs 'Qty.inBetween)
    , Class.static3 "Midpoints" "Start" "End" "N" (Qty.midpoints @SquareMeters) $(docs 'Qty.midpoints)
    , Class.factory1 "Square Meters" "Value" Area.squareMeters $(docs 'Area.squareMeters)
    , Class.factory1 "Square Inches" "Value" Area.squareInches $(docs 'Area.squareInches)
    , Class.member0 "In Square Meters" Area.inSquareMeters $(docs 'Area.inSquareMeters)
    , Class.member0 "In Square Inches" Area.inSquareInches $(docs 'Area.inSquareInches)
    , Class.memberS0 "Is Zero" (~= Area.zero) "Check if an area is zero, within the current tolerance."
    , Class.equalityAndHash
    , Class.comparison
    , Class.negateSelf
    , Class.absSelf Qty.abs
    , Class.floatTimes
    , Class.plusSelf
    , Class.plus @(Bounds SquareMeters) Self
    , Class.plus @(Curve SquareMeters) Self
    , Class.minusSelf
    , Class.minus @(Bounds SquareMeters) Self
    , Class.minus @(Curve SquareMeters) Self
    , Class.timesFloat
    , Class.times @(Bounds Unitless) Self
    , Class.times @(Curve Unitless) Self
    , Class.times @(Direction2d Space) Self
    , Class.times @(Vector2d (Space @ Unitless)) Self
    , Class.divByFloat
    , Class.divBySelf
    , Class.divBy @Length Self
    , Class.divBy @(Bounds Unitless) Self
    , Class.divBy @(Bounds Meters) Self
    , Class.divBy @(Bounds SquareMeters) Self
    , Class.divByU (\val crv -> Curve.quotient (Curve.constant val) crv)
    , Class.divByM (\val crv -> Curve.quotient (Curve.constant val) crv)
    , Class.divByS (\val crv -> Curve.quotient (Curve.constant val) crv)
    , Class.floorDivBySelf
    , Class.modBySelf
    ]

angle :: Class
angle =
  Class.new @Angle $(docs ''Angle) $
    [ Class.constant "Zero" Angle.zero $(docs 'Angle.zero)
    , Class.factory3 "Interpolate" "Start" "End" "Parameter Value" Qty.interpolateFrom $(docs 'Qty.interpolateFrom)
    , Class.static3 "Steps" "Start" "End" "N" (Qty.steps @Radians) $(docs 'Qty.steps)
    , Class.static3 "Leading" "Start" "End" "N" (Qty.leading @Radians) $(docs 'Qty.leading)
    , Class.static3 "Trailing" "Start" "End" "N" (Qty.trailing @Radians) $(docs 'Qty.trailing)
    , Class.static3 "In Between" "Start" "End" "N" (Qty.inBetween @Radians) $(docs 'Qty.inBetween)
    , Class.static3 "Midpoints" "Start" "End" "N" (Qty.midpoints @Radians) $(docs 'Qty.midpoints)
    , Class.constant "Golden Angle" Angle.goldenAngle $(docs 'Angle.goldenAngle)
    , Class.constant "Radian" Angle.radian $(docs 'Angle.radian)
    , Class.constant "Full Turn" Angle.fullTurn $(docs 'Angle.fullTurn)
    , Class.constant "Half Turn" Angle.halfTurn $(docs 'Angle.halfTurn)
    , Class.constant "Quarter Turn" Angle.quarterTurn $(docs 'Angle.quarterTurn)
    , Class.constant "Pi" Angle.pi $(docs 'Angle.pi)
    , Class.constant "Two Pi" Angle.twoPi $(docs 'Angle.twoPi)
    , Class.factory1 "Radians" "Value" Angle.radians $(docs 'Angle.radians)
    , Class.factory1 "Degrees" "Value" Angle.degrees $(docs 'Angle.degrees)
    , Class.factory1 "Turns" "Value" Angle.turns $(docs 'Angle.turns)
    , Class.factory1 "Acos" "Value" Angle.acos $(docs 'Angle.acos)
    , Class.factory1 "Asin" "Value" Angle.asin $(docs 'Angle.asin)
    , Class.factory1 "Atan" "Value" Angle.atan $(docs 'Angle.atan)
    , Class.member0 "In Radians" Angle.inRadians $(docs 'Angle.inRadians)
    , Class.member0 "In Degrees" Angle.inDegrees $(docs 'Angle.inDegrees)
    , Class.member0 "In Turns" Angle.inTurns $(docs 'Angle.inTurns)
    , Class.memberR0 "Is Zero" (~= Angle.zero) "Check if an angle is zero, within the current tolerance."
    , Class.member0 "Sin" Angle.sin $(docs 'Angle.sin)
    , Class.member0 "Cos" Angle.cos $(docs 'Angle.cos)
    , Class.member0 "Tan" Angle.tan $(docs 'Angle.tan)
    , Class.equalityAndHash
    , Class.comparison
    , Class.negateSelf
    , Class.absSelf Qty.abs
    , Class.floatTimes
    , Class.plusSelf
    , Class.plus @(Bounds Radians) Self
    , Class.plus @(Curve Radians) Self
    , Class.minusSelf
    , Class.minus @(Bounds Radians) Self
    , Class.minus @(Curve Radians) Self
    , Class.timesFloat
    , Class.times @(Bounds Unitless) Self
    , Class.times @(Curve Unitless) Self
    , Class.divByFloat
    , Class.divBySelf
    , Class.divBy @(Bounds Unitless) Self
    , Class.divBy @(Bounds Radians) Self
    , Class.divByU (\val crv -> Curve.quotient (Curve.constant val) crv)
    , Class.divByR (\val crv -> Curve.quotient (Curve.constant val) crv)
    , Class.floorDivBySelf
    , Class.modBySelf
    ]

bounds :: Class
bounds =
  Class.new @(Bounds Unitless) "A range of unitless values, with a lower bound and upper bound." $
    [ Class.constant "Unit Interval" Bounds.unitInterval $(docs 'Bounds.unitInterval)
    , Class.constructor2 "First Value" "Second Value" Bounds $(docs 'Bounds)
    , Class.factory1 "Constant" "Value" Bounds.constant $(docs 'Bounds.constant)
    , Class.factory1 "Zero To" "Value" Bounds.zeroTo $(docs 'Bounds.zeroTo)
    , Class.factory1 "Symmetric" "Width" Bounds.symmetric $(docs 'Bounds.symmetric)
    , Class.factory1 "Hull" "Values" Bounds.hullN $(docs 'Bounds.hullN)
    , Class.factory1 "Aggregate" "Bounds" Bounds.aggregateN $(docs 'Bounds.aggregateN)
    , Class.property "Endpoints" (.endpoints) $(docs 'Bounds.endpoints)
    , Class.property "Lower" (.lower) $(docs 'Bounds.lower)
    , Class.property "Upper" (.upper) $(docs 'Bounds.upper)
    , Class.member1 "Intersection" "Other" Bounds.intersection $(docs 'Bounds.intersection)
    , Class.member1 "Includes" "Value" Bounds.includes $(docs 'Bounds.includes)
    , Class.member1 "Contains" "Other" Bounds.contains $(docs 'Bounds.contains)
    , Class.negateSelf
    , Class.absSelf Bounds.abs
    , Class.floatPlus
    , Class.floatMinus
    , Class.floatTimes
    , Class.floatDivBy
    , Class.plusFloat
    , Class.plusSelf
    , Class.minusFloat
    , Class.minusSelf
    , Class.timesFloat
    , Class.timesSelf
    , Class.times @Length Self
    , Class.times @Area Self
    , Class.times @Angle Self
    , Class.times @(Bounds Meters) Self
    , Class.times @(Bounds SquareMeters) Self
    , Class.times @(Bounds Radians) Self
    , Class.divByFloat
    , Class.divBySelf
    ]

lengthBounds :: Class
lengthBounds =
  Class.new @(Bounds Meters) "A range of length values, with a lower bound and upper bound." $
    [ Class.constructor2 "First Value" "Second Value" Bounds $(docs 'Bounds)
    , Class.factory1 "Constant" "Value" Bounds.constant $(docs 'Bounds.constant)
    , Class.factory1 "Zero To" "Value" Bounds.zeroTo $(docs 'Bounds.zeroTo)
    , Class.factory1 "Symmetric" "Width" Bounds.symmetric $(docs 'Bounds.symmetric)
    , Class.factory1 "Hull" "Values" Bounds.hullN $(docs 'Bounds.hullN)
    , Class.factory1 "Aggregate" "Bounds" Bounds.aggregateN $(docs 'Bounds.aggregateN)
    , Class.property "Endpoints" (.endpoints) $(docs 'Bounds.endpoints)
    , Class.property "Lower" (.lower) $(docs 'Bounds.lower)
    , Class.property "Upper" (.upper) $(docs 'Bounds.upper)
    , Class.member1 "Intersection" "Other" Bounds.intersection $(docs 'Bounds.intersection)
    , Class.member1 "Includes" "Value" Bounds.includes $(docs 'Bounds.includes)
    , Class.member1 "Contains" "Other" Bounds.contains $(docs 'Bounds.contains)
    , Class.negateSelf
    , Class.absSelf Bounds.abs
    , Class.floatTimes
    , Class.plusSelf
    , Class.plus @Length Self
    , Class.minusSelf
    , Class.minus @Length Self
    , Class.timesFloat
    , Class.timesSelf
    , Class.times @Length Self
    , Class.times @(Bounds Unitless) Self
    , Class.divByFloat
    , Class.divBySelf
    , Class.divBy @Length Self
    , Class.divBy @(Bounds Unitless) Self
    ]

areaBounds :: Class
areaBounds =
  Class.new @(Bounds SquareMeters) "A range of area values, with a lower bound and upper bound." $
    [ Class.constructor2 "First Value" "Second Value" Bounds $(docs 'Bounds)
    , Class.factory1 "Constant" "Value" Bounds.constant $(docs 'Bounds.constant)
    , Class.factory1 "Zero To" "Value" Bounds.zeroTo $(docs 'Bounds.zeroTo)
    , Class.factory1 "Symmetric" "Width" Bounds.symmetric $(docs 'Bounds.symmetric)
    , Class.factory1 "Hull" "Values" Bounds.hullN $(docs 'Bounds.hullN)
    , Class.factory1 "Aggregate" "Bounds" Bounds.aggregateN $(docs 'Bounds.aggregateN)
    , Class.property "Endpoints" (.endpoints) $(docs 'Bounds.endpoints)
    , Class.property "Lower" (.lower) $(docs 'Bounds.lower)
    , Class.property "Upper" (.upper) $(docs 'Bounds.upper)
    , Class.member1 "Intersection" "Other" Bounds.intersection $(docs 'Bounds.intersection)
    , Class.member1 "Includes" "Value" Bounds.includes $(docs 'Bounds.includes)
    , Class.member1 "Contains" "Other" Bounds.contains $(docs 'Bounds.contains)
    , Class.negateSelf
    , Class.absSelf Bounds.abs
    , Class.floatTimes
    , Class.plusSelf
    , Class.plus @Area Self
    , Class.minusSelf
    , Class.minus @Area Self
    , Class.timesFloat
    , Class.times @(Bounds Unitless) Self
    , Class.divByFloat
    , Class.divBySelf
    , Class.divBy @Length Self
    , Class.divBy @Area Self
    , Class.divBy @(Bounds Unitless) Self
    , Class.divBy @(Bounds Meters) Self
    ]

angleBounds :: Class
angleBounds =
  Class.new @(Bounds Radians) "A range of angle values, with a lower bound and upper bound." $
    [ Class.constructor2 "First Value" "Second Value" Bounds $(docs 'Bounds)
    , Class.factory1 "Constant" "Value" Bounds.constant $(docs 'Bounds.constant)
    , Class.factory1 "Zero To" "Value" Bounds.zeroTo $(docs 'Bounds.zeroTo)
    , Class.factory1 "Symmetric" "Width" Bounds.symmetric $(docs 'Bounds.symmetric)
    , Class.factory1 "Hull" "Values" Bounds.hullN $(docs 'Bounds.hullN)
    , Class.factory1 "Aggregate" "Bounds" Bounds.aggregateN $(docs 'Bounds.aggregateN)
    , Class.property "Endpoints" (.endpoints) $(docs 'Bounds.endpoints)
    , Class.property "Lower" (.lower) $(docs 'Bounds.lower)
    , Class.property "Upper" (.upper) $(docs 'Bounds.upper)
    , Class.member1 "Intersection" "Other" Bounds.intersection $(docs 'Bounds.intersection)
    , Class.member1 "Includes" "Value" Bounds.includes $(docs 'Bounds.includes)
    , Class.member1 "Contains" "Other" Bounds.contains $(docs 'Bounds.contains)
    , Class.negateSelf
    , Class.absSelf Bounds.abs
    , Class.floatTimes
    , Class.plusSelf
    , Class.plus @Angle Self
    , Class.minusSelf
    , Class.minus @Angle Self
    , Class.timesFloat
    , Class.times @(Bounds Unitless) Self
    , Class.divByFloat
    , Class.divBySelf
    , Class.divBy @Angle Self
    , Class.divBy @(Bounds Unitless) Self
    ]

color :: Class
color =
  Class.new @Color $(docs ''Color) $
    [ Class.factory3 "RGB Float" "Red" "Green" "Blue" Color.rgbFloat $(docs 'Color.rgbFloat)
    , Class.factory3 "RGB Int" "Red" "Green" "Blue" Color.rgbInt $(docs 'Color.rgbInt)
    , Class.factory3 "HSL" "Hue" "Saturation" "Lightness" Color.hsl $(docs 'Color.hsl)
    , Class.factory1 "From Hex" "Hex String" Color.fromHex $(docs 'Color.fromHex)
    , Class.member0 "To Hex" Color.toHex $(docs 'Color.toHex)
    , Class.property "RGB Float Components" (.rgbFloatComponents) $(docs 'Color.rgbFloatComponents)
    , Class.property "RGB Int Components" (.rgbIntComponents) $(docs 'Color.rgbIntComponents)
    , Class.constant "Red" Color.red $(docs 'Color.red)
    , Class.constant "Dark Red" Color.darkRed $(docs 'Color.darkRed)
    , Class.constant "Light Orange" Color.lightOrange $(docs 'Color.lightOrange)
    , Class.constant "Orange" Color.orange $(docs 'Color.orange)
    , Class.constant "Dark Orange" Color.darkOrange $(docs 'Color.darkOrange)
    , Class.constant "Light Yellow" Color.lightYellow $(docs 'Color.lightYellow)
    , Class.constant "Yellow" Color.yellow $(docs 'Color.yellow)
    , Class.constant "Dark Yellow" Color.darkYellow $(docs 'Color.darkYellow)
    , Class.constant "Light Green" Color.lightGreen $(docs 'Color.lightGreen)
    , Class.constant "Green" Color.green $(docs 'Color.green)
    , Class.constant "Dark Green" Color.darkGreen $(docs 'Color.darkGreen)
    , Class.constant "Light Blue" Color.lightBlue $(docs 'Color.lightBlue)
    , Class.constant "Blue" Color.blue $(docs 'Color.blue)
    , Class.constant "Dark Blue" Color.darkBlue $(docs 'Color.darkBlue)
    , Class.constant "Light Purple" Color.lightPurple $(docs 'Color.lightPurple)
    , Class.constant "Purple" Color.purple $(docs 'Color.purple)
    , Class.constant "Dark Purple" Color.darkPurple $(docs 'Color.darkPurple)
    , Class.constant "Light Brown" Color.lightBrown $(docs 'Color.lightBrown)
    , Class.constant "Brown" Color.brown $(docs 'Color.brown)
    , Class.constant "Dark Brown" Color.darkBrown $(docs 'Color.darkBrown)
    , Class.constant "Black" Color.black $(docs 'Color.black)
    , Class.constant "White" Color.white $(docs 'Color.white)
    , Class.constant "Light Grey" Color.lightGrey $(docs 'Color.lightGrey)
    , Class.constant "Grey" Color.grey $(docs 'Color.grey)
    , Class.constant "Dark Grey" Color.darkGrey $(docs 'Color.darkGrey)
    , Class.constant "Light Gray" Color.lightGray $(docs 'Color.lightGray)
    , Class.constant "Gray" Color.gray $(docs 'Color.gray)
    , Class.constant "Dark Gray" Color.darkGray $(docs 'Color.darkGray)
    , Class.constant "Light Charcoal" Color.lightCharcoal $(docs 'Color.lightCharcoal)
    , Class.constant "Charcoal" Color.charcoal $(docs 'Color.charcoal)
    , Class.constant "Dark Charcoal" Color.darkCharcoal $(docs 'Color.darkCharcoal)
    ]

vector2d :: Class
vector2d =
  Class.new @(Vector2d (Space @ Unitless)) "A unitless vector in 2D." $
    [ Class.constant "Zero" (Vector2d.zero @Space @Unitless) $(docs 'Vector2d.zero)
    , Class.factory1 "Unit" "Direction" Vector2d.unit $(docs 'Vector2d.unit)
    , Class.constructor2 "X Component" "Y Component" Vector2d $(docs 'Vector2d)
    , Class.factory1 "Y" "Y Component" Vector2d.y $(docs 'Vector2d.y)
    , Class.factory1 "X" "X Component" Vector2d.x $(docs 'Vector2d.x)
    , Class.factory2 "Polar" "Magnitude" "Angle" Vector2d.polar $(docs 'Vector2d.polar)
    , Class.factory1 "From Components" "Components" Vector2d.fromComponents $(docs 'Vector2d.fromComponents)
    , Class.property "Components" (.components) $(docs 'Vector2d.components)
    , Class.property "X Component" (.xComponent) $(docs 'Vector2d.xComponent)
    , Class.property "Y Component" (.yComponent) $(docs 'Vector2d.yComponent)
    , Class.memberU0 "Direction" Vector2d.direction $(docs 'Vector2d.direction)
    , Class.member0 "Normalize" Vector2d.normalize $(docs 'Vector2d.normalize)
    , Class.property "Angle" (.angle) $(docs 'Vector2d.angle)
    , Class.member1 "Angle To" "Other" (flip Vector2d.angleFrom) $(docs 'Vector2d.angleFrom)
    , Class.memberU0 "Is Zero" (~= Vector2d.zero) "Check if a vector is zero, within the current tolerance."
    , Class.negateSelf
    , Class.floatTimes
    , Class.plusSelf
    , Class.minusSelf
    , Class.timesFloat
    , Class.times @Length Self
    , Class.times @Area Self
    , Class.divByFloat
    , Class.dotSelf
    , Class.dotProduct @(Vector2d (Space @ Meters)) Self
    , Class.dotProduct @(Vector2d (Space @ SquareMeters)) Self
    , Class.crossSelf
    , Class.crossProduct @(Vector2d (Space @ Meters)) Self
    , Class.crossProduct @(Vector2d (Space @ SquareMeters)) Self
    ]

displacement2d :: Class
displacement2d =
  Class.new @(Vector2d (Space @ Meters)) "A displacement vector in 2D." $
    [ Class.constant "Zero" (Vector2d.zero @Space @Meters) $(docs 'Vector2d.zero)
    , Class.constructor2 "X Component" "Y Component" Vector2d $(docs 'Vector2d)
    , Class.factory1 "X" "X Component" Vector2d.x $(docs 'Vector2d.x)
    , Class.factory1 "Y" "Y Component" Vector2d.y $(docs 'Vector2d.y)
    , Class.factory2 "Polar" "Magnitude" "Angle" Vector2d.polar $(docs 'Vector2d.polar)
    , Class.factory2 "Meters" "X Component" "Y Component" Vector2d.meters $(docs 'Vector2d.meters)
    , Class.factory2 "Centimeters" "X Component" "Y Component" Vector2d.centimeters $(docs 'Vector2d.centimeters)
    , Class.factory2 "Cm" "X Component" "Y Component" Vector2d.cm $(docs 'Vector2d.cm)
    , Class.factory2 "Millimeters" "X Component" "Y Component" Vector2d.millimeters $(docs 'Vector2d.millimeters)
    , Class.factory2 "Mm" "X Component" "Y Component" Vector2d.mm $(docs 'Vector2d.mm)
    , Class.factory2 "Inches" "X Component" "Y Component" Vector2d.inches $(docs 'Vector2d.inches)
    , Class.factory1 "From Components" "Components" Vector2d.fromComponents $(docs 'Vector2d.fromComponents)
    , Class.property "Components" (.components) $(docs 'Vector2d.components)
    , Class.property "X Component" (.xComponent) $(docs 'Vector2d.xComponent)
    , Class.property "Y Component" (.yComponent) $(docs 'Vector2d.yComponent)
    , Class.memberM0 "Direction" Vector2d.direction $(docs 'Vector2d.direction)
    , Class.member0 "Normalize" Vector2d.normalize $(docs 'Vector2d.normalize)
    , Class.property "Angle" (.angle) $(docs 'Vector2d.angle)
    , Class.member1 "Angle To" "Other" (flip Vector2d.angleFrom) $(docs 'Vector2d.angleFrom)
    , Class.memberM0 "Is Zero" (~= Vector2d.zero) "Check if a displacement is zero, within the current tolerance."
    , Class.negateSelf
    , Class.floatTimes
    , Class.plusSelf
    , Class.minusSelf
    , Class.timesFloat
    , Class.times @Length Self
    , Class.divByFloat
    , Class.divBy @Length Self
    , Class.dotSelf
    , Class.dotProduct @(Vector2d (Space @ Unitless)) Self
    , Class.crossSelf
    , Class.crossProduct @(Vector2d (Space @ Unitless)) Self
    ]

areaVector2d :: Class
areaVector2d =
  Class.new @(Vector2d (Space @ SquareMeters)) "A vector in 2D with units of area." $
    [ Class.constant "Zero" (Vector2d.zero @Space @SquareMeters) $(docs 'Vector2d.zero)
    , Class.constructor2 "X Component" "Y Component" Vector2d $(docs 'Vector2d)
    , Class.factory1 "X" "X Component" Vector2d.x $(docs 'Vector2d.x)
    , Class.factory1 "Y" "Y Component" Vector2d.y $(docs 'Vector2d.y)
    , Class.factory2 "Polar" "Magnitude" "Angle" Vector2d.polar $(docs 'Vector2d.polar)
    , Class.factory2 "Square Meters" "X Component" "Y Component" Vector2d.squareMeters $(docs 'Vector2d.squareMeters)
    , Class.factory1 "From Components" "Components" Vector2d.fromComponents $(docs 'Vector2d.fromComponents)
    , Class.property "Components" (.components) $(docs 'Vector2d.components)
    , Class.property "X Component" (.xComponent) $(docs 'Vector2d.xComponent)
    , Class.property "Y Component" (.yComponent) $(docs 'Vector2d.yComponent)
    , Class.memberS0 "Direction" Vector2d.direction $(docs 'Vector2d.direction)
    , Class.member0 "Normalize" Vector2d.normalize $(docs 'Vector2d.normalize)
    , Class.property "Angle" (.angle) $(docs 'Vector2d.angle)
    , Class.member1 "Angle To" "Other" (flip Vector2d.angleFrom) $(docs 'Vector2d.angleFrom)
    , Class.memberS0 "Is Zero" (~= Vector2d.zero) "Check if an area vector is zero, within the current tolerance."
    , Class.negateSelf
    , Class.floatTimes
    , Class.plusSelf
    , Class.minusSelf
    , Class.timesFloat
    , Class.divByFloat
    , Class.divBy @Length Self
    , Class.divBy @Area Self
    , Class.dotProduct @(Vector2d (Space @ Unitless)) Self
    , Class.crossProduct @(Vector2d (Space @ Unitless)) Self
    ]

direction2d :: Class
direction2d =
  Class.new @(Direction2d Space) $(docs ''Direction2d) $
    [ Class.upcast Vector2d.unit
    , Class.constant "X" Direction2d.x $(docs 'Direction2d.x)
    , Class.constant "Y" Direction2d.y $(docs 'Direction2d.y)
    , Class.factory1 "Polar" "Angle" Direction2d.polar $(docs 'Direction2d.polar)
    , Class.factory1 "Degrees" "Value" Direction2d.degrees $(docs 'Direction2d.degrees)
    , Class.factory1 "Radians" "Value" Direction2d.radians $(docs 'Direction2d.radians)
    , Class.property "Components" (.components) $(docs 'Direction2d.components)
    , Class.property "X Component" (.xComponent) $(docs 'Direction2d.xComponent)
    , Class.property "Y Component" (.yComponent) $(docs 'Direction2d.yComponent)
    , Class.member0 "Rotate Left" Direction2d.rotateLeft $(docs 'Direction2d.rotateLeft)
    , Class.member0 "Rotate Right" Direction2d.rotateRight $(docs 'Direction2d.rotateRight)
    , Class.negateSelf
    ]

point2d :: Class
point2d =
  Class.new @(Point2d (Space @ Meters)) "A point in 2D, defined by its X and Y coordinates." $
    [ Class.constant "Origin" (Point2d.origin @Space @Meters) $(docs 'Point2d.origin)
    , Class.constructor2 "X Coordinate" "Y Coordinate" Point2d $(docs 'Point2d)
    , Class.factory1 "X" "X Coordinate" Point2d.x $(docs 'Point2d.x)
    , Class.factory1 "Y" "Y Coordinate" Point2d.y $(docs 'Point2d.y)
    , Class.factory2 "Polar" "Radius" "Angle" Point2d.polar $(docs 'Point2d.polar)
    , Class.factory2 "Meters" "X Coordinate" "Y Coordinate" Point2d.meters $(docs 'Point2d.meters)
    , Class.factory2 "Centimeters" "X Coordinate" "Y Coordinate" Point2d.centimeters $(docs 'Point2d.centimeters)
    , Class.factory2 "Cm" "X Coordinate" "Y Coordinate" Point2d.cm $(docs 'Point2d.cm)
    , Class.factory2 "Millimeters" "X Coordinate" "Y Coordinate" Point2d.millimeters $(docs 'Point2d.millimeters)
    , Class.factory2 "Mm" "X Coordinate" "Y Coordinate" Point2d.mm $(docs 'Point2d.mm)
    , Class.factory2 "Inches" "X Coordinate" "Y Coordinate" Point2d.inches $(docs 'Point2d.inches)
    , Class.factory1 "From Coordinates" "Coordinates" Point2d.fromCoordinates $(docs 'Point2d.fromCoordinates)
    , Class.property "Coordinates" (.coordinates) $(docs 'Point2d.coordinates)
    , Class.property "X Coordinate" (.xCoordinate) $(docs 'Point2d.xCoordinate)
    , Class.property "Y Coordinate" (.yCoordinate) $(docs 'Point2d.yCoordinate)
    , Class.member1 "Distance To" "Other" Point2d.distanceFrom $(docs 'Point2d.distanceFrom)
    , Class.member1 "Midpoint" "Other" Point2d.midpoint $(docs 'Point2d.midpoint)
    , Class.member1 "On" "Plane" Point2d.on $(docs 'Point2d.on)
    , Class.minusSelf
    , Class.minus @(Vector2d (Space @ Meters)) Self
    , Class.plus @(Vector2d (Space @ Meters)) Self
    , Class.minus @(Curve2d (Space @ Meters)) Self
    ]
      <> affineTransformations2d Point2d.transformBy

uvPoint :: Class
uvPoint =
  Class.new @(Point2d (Space @ Unitless)) "A point in UV parameter space." $
    [ Class.constant "Origin" (Point2d.origin @Space @Unitless) "The point with coordinates (0,0)."
    , Class.constructor2 "U Coordinate" "V Coordinate" Point2d "Construct a point from its U and V coordinates."
    , Class.factory1 "From Coordinates" "Coordinates" Point2d.fromCoordinates "Construct a point from a pair of U and V coordinates."
    , Class.property "Coordinates" (.coordinates) "Get the U and V coordinates of a point."
    , Class.property "U Coordinate" (.xCoordinate) "Get the U coordinate of a point."
    , Class.property "V Coordinate" (.yCoordinate) "Get the V coordinate of a point."
    , Class.member1 "Distance To" "Other" Point2d.distanceFrom $(docs 'Point2d.distanceFrom)
    , Class.member1 "Midpoint" "Other" Point2d.midpoint $(docs 'Point2d.midpoint)
    , Class.minusSelf
    , Class.minus @(Vector2d (Space @ Unitless)) Self
    , Class.plus @(Vector2d (Space @ Unitless)) Self
    ]
      <> affineTransformations2d Point2d.transformBy

bounds2d :: Class
bounds2d =
  Class.new @(Bounds2d (Space @ Meters)) "A bounding box in 2D." $
    [ Class.constructor2 "X Coordinate" "Y Coordinate" Bounds2d $(docs 'Bounds2d)
    , Class.factory1 "Constant" "Point" Bounds2d.constant $(docs 'Bounds2d.constant)
    , Class.factory2 "From Corners" "First Point" "Second Point" Bounds2d.hull2 $(docs 'Bounds2d.hull2)
    , Class.factory1 "Hull" "Points" (Bounds2d.hullN @(Point2d (Space @ Meters))) $(docs 'Bounds2d.hullN)
    , Class.factory1 "Aggregate" "Bounds" Bounds2d.aggregateN $(docs 'Bounds2d.aggregateN)
    , Class.property "Coordinates" (.coordinates) $(docs 'Bounds2d.coordinates)
    , Class.property "X Coordinate" (.xCoordinate) $(docs 'Bounds2d.xCoordinate)
    , Class.property "Y Coordinate" (.yCoordinate) $(docs 'Bounds2d.yCoordinate)
    , Class.plus @(Vector2d (Space @ Meters)) Self
    , Class.minus @(Vector2d (Space @ Meters)) Self
    ]
      <> affineTransformations2d Bounds2d.transformBy

uvBounds :: Class
uvBounds =
  Class.new @(Bounds2d (Space @ Unitless)) "A bounding box in UV parameter space." $
    [ Class.constructor2 "U Coordinate" "V Coordinate" Bounds2d "Construct a bounding box from its U and V coordinate bounds."
    , Class.factory1 "Constant" "Point" Bounds2d.constant $(docs 'Bounds2d.constant)
    , Class.factory2 "From Corners" "First Point" "Second Point" Bounds2d.hull2 $(docs 'Bounds2d.hull2)
    , Class.factory1 "Hull" "Points" (Bounds2d.hullN @(Point2d (Space @ Unitless))) $(docs 'Bounds2d.hullN)
    , Class.factory1 "Aggregate" "Bounds" Bounds2d.aggregateN $(docs 'Bounds2d.aggregateN)
    , Class.property "Coordinates" (.coordinates) $(docs 'Bounds2d.coordinates)
    , Class.property "U Coordinate" (.xCoordinate) "Get the U coordinate bounds of a bounding box."
    , Class.property "V Coordinate" (.yCoordinate) "Get the V coordinate bounds of a bounding box."
    , Class.plus @(Vector2d (Space @ Unitless)) Self
    , Class.minus @(Vector2d (Space @ Unitless)) Self
    ]
      <> affineTransformations2d Bounds2d.transformBy

curve :: Class
curve =
  Class.new @(Curve Unitless) "A parametric curve definining a unitless value in terms of a parameter value." $
    [ Class.constant "Zero" (Curve.zero @Unitless) $(docs 'Curve.zero)
    , Class.constant "T" Curve.t $(docs 'Curve.t)
    , Class.factory1 "Constant" "Value" Curve.constant $(docs 'Curve.constant)
    , Class.factory2 "Line" "Start" "End" Curve.line $(docs 'Curve.line)
    , Class.member0 "Squared" Curve.squared $(docs 'Curve.squared)
    , Class.memberU0 "Sqrt" Curve.sqrt $(docs 'Curve.sqrt)
    , Class.member1 "Evaluate" "Parameter Value" (flip Curve.evaluate) $(docs 'Curve.evaluate)
    , Class.memberU0 "Zeros" Curve.zeros $(docs 'Curve.zeros)
    , Class.memberU0 "Is Zero" (~= 0.0) "Check if a curve is zero everywhere, within the current tolerance."
    , Class.negateSelf
    , Class.floatPlus
    , Class.floatMinus
    , Class.floatTimes
    , Class.floatDivByU (\val crv -> Curve.quotient (Curve.constant val) crv)
    , Class.plusFloat
    , Class.plusSelf
    , Class.minusFloat
    , Class.minusSelf
    , Class.timesFloat
    , Class.timesSelf
    , Class.times @Length Self
    , Class.times @Area Self
    , Class.times @Angle Self
    , Class.times @(Curve Meters) Self
    , Class.times @(Curve SquareMeters) Self
    , Class.times @(Curve Radians) Self
    , Class.divByFloat
    , Class.divByU Curve.quotient
    , Class.nested @Curve.Zero "A point where a given curve is equal to zero." $
        [ Class.property "Location" (.location) "The parameter value at which the curve is zero."
        , Class.property "Order" (.order) "The order of the solution: 0 for crossing, 1 for tangent, etc."
        , Class.property "Sign" (.sign) "The sign of the solution: the sign of the curve to the right of the solution."
        ]
    ]

angleCurve :: Class
angleCurve =
  Class.new @(Curve Radians) "A parametric curve definining an angle in terms of a parameter value." $
    [ Class.constant "Zero" (Curve.zero @Radians) $(docs 'Curve.zero)
    , Class.factory1 "Constant" "Value" Curve.constant $(docs 'Curve.constant)
    , Class.factory2 "Line" "Start" "End" Curve.line $(docs 'Curve.line)
    , Class.member0 "Sin" Curve.sin $(docs 'Curve.sin)
    , Class.member0 "Cos" Curve.cos $(docs 'Curve.cos)
    , Class.member1 "Evaluate" "Parameter Value" (flip Curve.evaluate) $(docs 'Curve.evaluate)
    , Class.memberR0 "Zeros" Curve.zeros $(docs 'Curve.zeros)
    , Class.memberR0 "Is Zero" (~= Angle.zero) "Check if a curve is zero everywhere, within the current tolerance."
    , Class.negateSelf
    , Class.floatTimes
    , Class.plusSelf
    , Class.plus @Angle Self
    , Class.minusSelf
    , Class.minus @Angle Self
    , Class.timesFloat
    , Class.times @(Curve Unitless) Self
    , Class.divByFloat
    , Class.divByR Curve.quotient
    , Class.divBy @Angle Self
    , Class.divByU Curve.quotient
    ]

lengthCurve :: Class
lengthCurve =
  Class.new @(Curve Meters) "A parametric curve definining a length in terms of a parameter value." $
    [ Class.constant "Zero" (Curve.zero @Meters) $(docs 'Curve.zero)
    , Class.factory1 "Constant" "Value" Curve.constant $(docs 'Curve.constant)
    , Class.factory2 "Line" "Start" "End" Curve.line $(docs 'Curve.line)
    , Class.member0 "Squared" Curve.squared $(docs 'Curve.squared)
    , Class.member1 "Evaluate" "Parameter Value" (flip Curve.evaluate) $(docs 'Curve.evaluate)
    , Class.memberM0 "Zeros" Curve.zeros $(docs 'Curve.zeros)
    , Class.memberM0 "Is Zero" (~= Length.zero) "Check if a curve is zero everywhere, within the current tolerance."
    , Class.negateSelf
    , Class.floatTimes
    , Class.plusSelf
    , Class.plus @Length Self
    , Class.minusSelf
    , Class.minus @Length Self
    , Class.timesFloat
    , Class.timesSelf
    , Class.times @Length Self
    , Class.times @(Curve Unitless) Self
    , Class.divByFloat
    , Class.divByM Curve.quotient
    , Class.divBy @Length Self
    , Class.divByU Curve.quotient
    ]

areaCurve :: Class
areaCurve =
  Class.new @(Curve SquareMeters) "A parametric curve definining an area in terms of a parameter value." $
    [ Class.constant "Zero" (Curve.zero @SquareMeters) $(docs 'Curve.zero)
    , Class.factory1 "Constant" "Value" Curve.constant $(docs 'Curve.constant)
    , Class.factory2 "Line" "Start" "End" Curve.line $(docs 'Curve.line)
    , Class.memberM0 "Sqrt" Curve.sqrt $(docs 'Curve.sqrt)
    , Class.member1 "Evaluate" "Parameter Value" (flip Curve.evaluate) $(docs 'Curve.evaluate)
    , Class.memberS0 "Zeros" Curve.zeros $(docs 'Curve.zeros)
    , Class.memberS0 "Is Zero" (~= Area.zero) "Check if a curve is zero everywhere, within the current tolerance."
    , Class.negateSelf
    , Class.floatTimes
    , Class.plusSelf
    , Class.plus @Area Self
    , Class.minusSelf
    , Class.minus @Area Self
    , Class.timesFloat
    , Class.times @(Curve Unitless) Self
    , Class.divByFloat
    , Class.divByS Curve.quotient
    , Class.divBy @Length Self
    , Class.divBy @Area Self
    , Class.divByU Curve.quotient
    , Class.divByM Curve.quotient
    ]

drawing2d :: Class
drawing2d =
  Class.new @(Drawing2d Space) $(docs ''Drawing2d) $
    [ Class.member1 "To SVG" "View Box" Drawing2d.toSvg $(docs 'Drawing2d.toSvg)
    , Class.member2 "Write SVG" "Path" "View Box" Drawing2d.writeSvg $(docs 'Drawing2d.writeSvg)
    , Class.factory1 "Group" "Drawings" Drawing2d.group $(docs 'Drawing2d.group)
    , Class.factory2 "Group With" "Attributes" "Drawings" Drawing2d.groupWith $(docs 'Drawing2d.groupWith)
    , Class.factory1 "Polygon" "Vertices" Drawing2d.polygon $(docs 'Drawing2d.polygon)
    , Class.factory2 "Polygon With" "Attributes" "Vertices" Drawing2d.polygonWith $(docs 'Drawing2d.polygonWith)
    , Class.factory2 "Circle" "Center Point" "Diameter" (Class.curryT2 Drawing2d.circle) $(docs 'Drawing2d.circle)
    , Class.factory3 "Circle With" "Attributes" "Center Point" "Diameter" (Class.curry1T2 Drawing2d.circleWith) $(docs 'Drawing2d.circleWith)
    , Class.factory2 "Curve" "Resolution" "Curve" Drawing2d.curve $(docs 'Drawing2d.curve)
    , Class.factory3 "Curve With" "Attributes" "Resolution" "Curve" Drawing2d.curveWith $(docs 'Drawing2d.curveWith)
    , Class.constant "Black Stroke" Drawing2d.blackStroke $(docs 'Drawing2d.blackStroke)
    , Class.static1 "Stroke Color" "Color" Drawing2d.strokeColor $(docs 'Drawing2d.strokeColor)
    , Class.constant "No Fill" Drawing2d.noFill $(docs 'Drawing2d.noFill)
    , Class.static1 "Fill Color" "Color" Drawing2d.fillColor $(docs 'Drawing2d.fillColor)
    , Class.nested @(Drawing2d.Attribute Space) "A drawing attribute such as fill color or stroke width." []
    ]

axis2d :: Class
axis2d =
  Class.new @(Axis2d (Space @ Meters)) $(docs ''Axis2d) $
    [ Class.constructor2 "Origin Point" "Direction" Axis2d $(docs 'Axis2d)
    , Class.property "Origin Point" (.originPoint) $(docs 'Axis2d.originPoint)
    , Class.property "Direction" (.direction) $(docs 'Axis2d.direction)
    , Class.constant "X" (Axis2d.x @Space @Meters) $(docs 'Axis2d.x)
    , Class.constant "Y" (Axis2d.y @Space @Meters) $(docs 'Axis2d.y)
    ]
      <> orthonormalTransformations2d Axis2d.transformBy

uvAxis :: Class
uvAxis =
  Class.new @(Axis2d (Space @ Unitless)) $(docs ''Axis2d) $
    [ Class.constructor2 "Origin Point" "Direction" Axis2d $(docs 'Axis2d)
    , Class.property "Origin Point" (.originPoint) $(docs 'Axis2d.originPoint)
    , Class.property "Direction" (.direction) $(docs 'Axis2d.direction)
    , Class.constant "U" (Axis2d.x @Space @Meters) "The U axis."
    , Class.constant "V" (Axis2d.y @Space @Meters) "The V axis."
    ]

convention3d :: Class
convention3d =
  Class.new @Convention3d $(docs ''Convention3d) $
    [ Class.constant "Y Up" Convention3d.yUp $(docs 'Convention3d.yUp)
    , Class.constant "Z Up" Convention3d.zUp $(docs 'Convention3d.zUp)
    ]

vector3d :: Class
vector3d =
  Class.new @(Vector3d (Space @ Unitless)) "A unitless vector in 3D." $
    [ Class.constant "Zero" (Vector3d.zero @Space @Unitless) $(docs 'Vector3d.zero)
    , Class.factory1 "Unit" "Direction" Vector3d.unit $(docs 'Vector3d.unit)
    , Class.factory2 "From Components" "Convention" "Components" Vector3d.fromComponents $(docs 'Vector3d.fromComponents)
    , Class.member1 "Components" "Convention" Vector3d.components $(docs 'Vector3d.components)
    , Class.memberU0 "Direction" Vector3d.direction $(docs 'Vector3d.direction)
    , Class.memberU0 "Is Zero" (~= Vector3d.zero) "Check if a vector is zero, within the current tolerance."
    , Class.member2 "Rotate In" "Direction" "Angle" Vector3d.rotateIn $(docs 'Vector3d.rotateIn)
    , Class.member2 "Rotate Around" "Axis" "Angle" (Vector3d.rotateAround @Space @Meters) $(docs 'Vector3d.rotateAround)
    , Class.member1 "Mirror In" "Direction" Vector3d.mirrorIn $(docs 'Vector3d.mirrorIn)
    , Class.member1 "Mirror Across" "Plane" (Vector3d.mirrorAcross @Space @Meters) $(docs 'Vector3d.mirrorAcross)
    , Class.member2 "Scale In" "Direction" "Scale" Vector3d.scaleIn $(docs 'Vector3d.scaleIn)
    , Class.member2 "Scale Along" "Axis" "Scale" (Vector3d.scaleAlong @Space @Meters) $(docs 'Vector3d.scaleAlong)
    , Class.member1 "Place In" "Frame" (Vector3d.placeIn @Space @Meters) $(docs 'Vector3d.placeIn)
    , Class.member1 "Relative To" "Frame" (Vector3d.relativeTo @Space @Meters) $(docs 'Vector3d.relativeTo)
    , Class.negateSelf
    , Class.floatTimes
    , Class.plusSelf
    , Class.minusSelf
    , Class.timesFloat
    , Class.times @Length Self
    , Class.times @Area Self
    , Class.divByFloat
    , Class.dotSelf
    , Class.dotProduct @(Vector3d (Space @ Meters)) Self
    , Class.dotProduct @(Vector3d (Space @ SquareMeters)) Self
    , Class.crossSelf
    , Class.crossProduct @(Vector3d (Space @ Meters)) Self
    , Class.crossProduct @(Vector3d (Space @ SquareMeters)) Self
    ]

displacement3d :: Class
displacement3d =
  Class.new @(Vector3d (Space @ Meters)) "A displacement vector in 3D." $
    [ Class.constant "Zero" (Vector3d.zero @Space @Meters) $(docs 'Vector3d.zero)
    , Class.factory2 "From Components" "Convention" "Components" Vector3d.fromComponents $(docs 'Vector3d.fromComponents)
    , Class.member1 "Components" "Convention" Vector3d.components $(docs 'Vector3d.components)
    , Class.memberM0 "Direction" Vector3d.direction $(docs 'Vector3d.direction)
    , Class.memberM0 "Is Zero" (~= Vector3d.zero) "Check if a displacement is zero, within the current tolerance."
    , Class.member2 "Rotate In" "Direction" "Angle" Vector3d.rotateIn $(docs 'Vector3d.rotateIn)
    , Class.member2 "Rotate Around" "Axis" "Angle" (Vector3d.rotateAround @Space @Meters) $(docs 'Vector3d.rotateAround)
    , Class.member1 "Mirror In" "Direction" Vector3d.mirrorIn $(docs 'Vector3d.mirrorIn)
    , Class.member1 "Mirror Across" "Plane" (Vector3d.mirrorAcross @Space @Meters) $(docs 'Vector3d.mirrorAcross)
    , Class.member2 "Scale In" "Direction" "Scale" Vector3d.scaleIn $(docs 'Vector3d.scaleIn)
    , Class.member2 "Scale Along" "Axis" "Scale" (Vector3d.scaleAlong @Space @Meters) $(docs 'Vector3d.scaleAlong)
    , Class.member1 "Place In" "Frame" (Vector3d.placeIn @Space @Meters) $(docs 'Vector3d.placeIn)
    , Class.member1 "Relative To" "Frame" (Vector3d.relativeTo @Space @Meters) $(docs 'Vector3d.relativeTo)
    , Class.negateSelf
    , Class.floatTimes
    , Class.plusSelf
    , Class.minusSelf
    , Class.timesFloat
    , Class.times @Length Self
    , Class.divByFloat
    , Class.divBy @Length Self
    , Class.dotSelf
    , Class.dotProduct @(Vector3d (Space @ Unitless)) Self
    , Class.crossSelf
    , Class.crossProduct @(Vector3d (Space @ Unitless)) Self
    ]

areaVector3d :: Class
areaVector3d =
  Class.new @(Vector3d (Space @ SquareMeters)) "A vector in 3D with units of area." $
    [ Class.constant "Zero" (Vector3d.zero @Space @SquareMeters) $(docs 'Vector3d.zero)
    , Class.factory2 "From Components" "Convention" "Components" Vector3d.fromComponents $(docs 'Vector3d.fromComponents)
    , Class.member1 "Components" "Convention" Vector3d.components $(docs 'Vector3d.components)
    , Class.memberS0 "Direction" Vector3d.direction $(docs 'Vector3d.direction)
    , Class.memberS0 "Is Zero" (~= Vector3d.zero) "Check if an area vector is zero, within the current tolerance."
    , Class.member2 "Rotate In" "Direction" "Angle" Vector3d.rotateIn $(docs 'Vector3d.rotateIn)
    , Class.member2 "Rotate Around" "Axis" "Angle" (Vector3d.rotateAround @Space @Meters) $(docs 'Vector3d.rotateAround)
    , Class.member1 "Mirror In" "Direction" Vector3d.mirrorIn $(docs 'Vector3d.mirrorIn)
    , Class.member1 "Mirror Across" "Plane" (Vector3d.mirrorAcross @Space @Meters) $(docs 'Vector3d.mirrorAcross)
    , Class.member2 "Scale In" "Direction" "Scale" Vector3d.scaleIn $(docs 'Vector3d.scaleIn)
    , Class.member2 "Scale Along" "Axis" "Scale" (Vector3d.scaleAlong @Space @Meters) $(docs 'Vector3d.scaleAlong)
    , Class.member1 "Place In" "Frame" (Vector3d.placeIn @Space @Meters) $(docs 'Vector3d.placeIn)
    , Class.member1 "Relative To" "Frame" (Vector3d.relativeTo @Space @Meters) $(docs 'Vector3d.relativeTo)
    , Class.negateSelf
    , Class.floatTimes
    , Class.plusSelf
    , Class.minusSelf
    , Class.timesFloat
    , Class.divByFloat
    , Class.divBy @Length Self
    , Class.divBy @Area Self
    , Class.dotProduct @(Vector3d (Space @ Unitless)) Self
    , Class.crossProduct @(Vector3d (Space @ Unitless)) Self
    ]

direction3d :: Class
direction3d =
  Class.new @(Direction3d Space) $(docs ''Direction3d) $
    [ Class.upcast Vector3d.unit
    , Class.member0 "Perpendicular Direction" Direction3d.perpendicularDirection $(docs 'Direction3d.perpendicularDirection)
    , Class.member1 "Angle To" "Other" Direction3d.angleFrom $(docs 'Direction3d.angleFrom)
    , Class.member2 "Rotate In" "Direction" "Angle" Direction3d.rotateIn $(docs 'Direction3d.rotateIn)
    , Class.member2 "Rotate Around" "Axis" "Angle" (Direction3d.rotateAround @Space @Meters) $(docs 'Direction3d.rotateAround)
    , Class.member1 "Mirror In" "Direction" Direction3d.mirrorIn $(docs 'Direction3d.mirrorIn)
    , Class.member1 "Mirror Across" "Plane" (Direction3d.mirrorAcross @Space @Meters) $(docs 'Direction3d.mirrorAcross)
    , Class.member1 "Place In" "Frame" (Direction3d.placeIn @Space @Meters) $(docs 'Direction3d.placeIn)
    , Class.member1 "Relative To" "Frame" (Direction3d.relativeTo @Space @Meters) $(docs 'Direction3d.relativeTo)
    , Class.negateSelf
    ]

point3d :: Class
point3d =
  Class.new @(Point3d (Space @ Meters)) "A point in 3D." $
    [ Class.factory2 "Along" "Axis" "Distance" Point3d.along $(docs 'Point3d.along)
    , Class.factory2 "On" "Plane" "Position" Point3d.on $(docs 'Point3d.on)
    , Class.factory2 "From Coordinates" "Convention" "Coordinates" Point3d.fromCoordinates $(docs 'Point3d.fromCoordinates)
    , Class.member1 "Coordinates" "Convention" Point3d.coordinates $(docs 'Point3d.coordinates)
    , Class.member1 "Distance To" "Other" Point3d.distanceFrom $(docs 'Point3d.distanceFrom)
    , Class.member1 "Midpoint" "Other" Point3d.midpoint $(docs 'Point3d.midpoint)
    , Class.member1 "Project Onto" "Plane" Point3d.projectOnto $(docs 'Point3d.projectOnto)
    , Class.member1 "Project Into" "Plane" Point3d.projectInto $(docs 'Point3d.projectInto)
    , Class.minusSelf
    , Class.minus @(Vector3d (Space @ Meters)) Self
    , Class.plus @(Vector3d (Space @ Meters)) Self
    , Class.member1 "Place In" "Frame" Point3d.placeIn $(docs 'Point3d.placeIn)
    , Class.member1 "Relative To" "Frame" Point3d.relativeTo $(docs 'Point3d.relativeTo)
    ]
      <> affineTransformations3d Point3d.transformBy

bounds3d :: Class
bounds3d =
  Class.new @(Bounds3d (Space @ Meters)) "A bounding box in 3D." $
    [ Class.factory1 "Constant" "Point" Bounds3d.constant $(docs 'Bounds3d.constant)
    , Class.factory2 "From Corners" "First Point" "Second Point" Bounds3d.hull2 $(docs 'Bounds3d.hull2)
    , Class.factory1 "Hull" "Points" (Bounds3d.hullN @(Point3d (Space @ Meters))) $(docs 'Bounds3d.hullN)
    , Class.factory1 "Aggregate" "Bounds" Bounds3d.aggregateN $(docs 'Bounds3d.aggregateN)
    , Class.member1 "Coordinates" "Convention" Bounds3d.coordinates $(docs 'Bounds3d.coordinates)
    , Class.plus @(Vector3d (Space @ Meters)) Self
    , Class.minus @(Vector3d (Space @ Meters)) Self
    ]
      <> affineTransformations3d Bounds3d.transformBy

axis3d :: Class
axis3d =
  Class.new @(Axis3d (Space @ Meters)) $(docs ''Axis3d) $
    [ Class.property "Origin Point" (.originPoint) $(docs 'Axis3d.originPoint)
    , Class.property "Direction" (.direction) $(docs 'Axis3d.direction)
    , Class.member0 "Normal Plane" Axis3d.normalPlane $(docs 'Axis3d.normalPlane)
    , Class.member1 "Move To" "Point" Axis3d.moveTo $(docs 'Axis3d.moveTo)
    , Class.member0 "Reverse" Axis3d.reverse $(docs 'Axis3d.reverse)
    , Class.member1 "Place In" "Frame" Axis3d.placeIn $(docs 'Axis3d.placeIn)
    , Class.member1 "Relative To" "Frame" Axis3d.relativeTo $(docs 'Axis3d.relativeTo)
    ]
      <> orthonormalTransformations3d Axis3d.transformBy

planeOrientation3d :: Class
planeOrientation3d =
  Class.new @(PlaneOrientation3d Space) $(docs ''PlaneOrientation3d) $
    [ Class.factory1 "From Normal Direction" "Direction" PlaneOrientation3d.fromNormalDirection $(docs 'PlaneOrientation3d.fromNormalDirection)
    , Class.factory1 "From X Direction" "Direction" PlaneOrientation3d.fromXDirection $(docs 'PlaneOrientation3d.fromXDirection)
    , Class.factory1 "From Y Direction" "Direction" PlaneOrientation3d.fromYDirection $(docs 'PlaneOrientation3d.fromYDirection)
    , Class.property "X Direction" (.xDirection) $(docs 'PlaneOrientation3d.xDirection)
    , Class.property "Y Direction" (.yDirection) $(docs 'PlaneOrientation3d.yDirection)
    , Class.property "Normal Direction" (.normalDirection) $(docs 'PlaneOrientation3d.normalDirection)
    , Class.member1 "Place In" "Frame" (PlaneOrientation3d.placeIn @Space @Meters) $(docs 'PlaneOrientation3d.placeIn)
    , Class.member1 "Relative To" "Frame" (PlaneOrientation3d.relativeTo @Space @Meters) $(docs 'PlaneOrientation3d.relativeTo)
    ]

plane3d :: Class
plane3d =
  Class.new @(Plane3d (Space @ Meters) (Defines Space)) $(docs ''Plane3d) $
    [ Class.factory2 "From Point And Normal" "Origin Point" "Normal Direction" Plane3d.fromPointAndNormal $(docs 'Plane3d.fromPointAndNormal)
    , Class.factory1 "From X Axis" "Axis" Plane3d.fromXAxis $(docs 'Plane3d.fromXAxis)
    , Class.factory1 "From Y Axis" "Axis" Plane3d.fromYAxis $(docs 'Plane3d.fromYAxis)
    , Class.property "Origin Point" (.originPoint) $(docs 'Plane3d.originPoint)
    , Class.property "X Direction" (.xDirection) $(docs 'Plane3d.xDirection)
    , Class.property "Y Direction" (.yDirection) $(docs 'Plane3d.yDirection)
    , Class.property "Normal Direction" (.normalDirection) $(docs 'Plane3d.normalDirection)
    , Class.property "X Axis" (.xAxis) $(docs 'Plane3d.xAxis)
    , Class.property "Y Axis" (.yAxis) $(docs 'Plane3d.yAxis)
    , Class.property "Normal Axis" (.normalAxis) $(docs 'Plane3d.normalAxis)
    , Class.member1 "Move To" "Point" Plane3d.moveTo $(docs 'Plane3d.moveTo)
    , Class.member0 "Flip" Plane3d.flip $(docs 'Plane3d.flip)
    , Class.member1 "Offset By" "Distance" Plane3d.offsetBy $(docs 'Plane3d.offsetBy)
    , Class.member1 "Place In" "Frame" Plane3d.placeIn $(docs 'Plane3d.placeIn)
    , Class.member1 "Relative To" "Frame" Plane3d.relativeTo $(docs 'Plane3d.relativeTo)
    ]
      <> rigidTransformations3d Plane3d.transformBy

orientation3d :: Class
orientation3d =
  Class.new @(Orientation3d Space) $(docs ''Orientation3d) $
    [ Class.constant "World" (Orientation3d.world @Space) $(docs 'Orientation3d.world)
    , Class.property "Forward Direction" (.forwardDirection) $(docs 'Orientation3d.forwardDirection)
    , Class.property "Backward Direction" (.backwardDirection) $(docs 'Orientation3d.backwardDirection)
    , Class.property "Leftward Direction" (.leftwardDirection) $(docs 'Orientation3d.leftwardDirection)
    , Class.property "Rightward Direction" (.rightwardDirection) $(docs 'Orientation3d.rightwardDirection)
    , Class.property "Upward Direction" (.upwardDirection) $(docs 'Orientation3d.upwardDirection)
    , Class.property "Downward Direction" (.downwardDirection) $(docs 'Orientation3d.downwardDirection)
    , Class.property "Front Plane Orientation" (.frontPlaneOrientation) $(docs 'Orientation3d.frontPlaneOrientation)
    , Class.property "Back Plane Orientation" (.backPlaneOrientation) $(docs 'Orientation3d.backPlaneOrientation)
    , Class.property "Right Plane Orientation" (.rightPlaneOrientation) $(docs 'Orientation3d.rightPlaneOrientation)
    , Class.property "Left Plane Orientation" (.leftPlaneOrientation) $(docs 'Orientation3d.leftPlaneOrientation)
    , Class.property "Bottom Plane Orientation" (.bottomPlaneOrientation) $(docs 'Orientation3d.bottomPlaneOrientation)
    , Class.property "Top Plane Orientation" (.topPlaneOrientation) $(docs 'Orientation3d.topPlaneOrientation)
    , Class.property "Backward Orientation" (.backwardOrientation) $(docs 'Orientation3d.backwardOrientation)
    , Class.property "Rightward Orientation" (.rightwardOrientation) $(docs 'Orientation3d.rightwardOrientation)
    , Class.property "Leftward Orientation" (.leftwardOrientation) $(docs 'Orientation3d.leftwardOrientation)
    , Class.property "Upward Orientation" (.upwardOrientation) $(docs 'Orientation3d.upwardOrientation)
    , Class.property "Downward Orientation" (.downwardOrientation) $(docs 'Orientation3d.downwardOrientation)
    , Class.member1 "Place In" "Frame" (Orientation3d.placeIn @Space @Meters) $(docs 'Orientation3d.placeIn)
    , Class.member1 "Relative To" "Frame" (Orientation3d.relativeTo @Space @Meters) $(docs 'Orientation3d.relativeTo)
    ]

frame3d :: Class
frame3d =
  Class.new @(Frame3d (Space @ Meters) (Defines Space)) $(docs ''Frame3d) $
    [ Class.constant "World" (Frame3d.world @Space @Meters) $(docs 'Frame3d.world)
    , Class.factory1 "Forward" "Reference Frame" Frame3d.forward $(docs 'Frame3d.forward)
    , Class.factory1 "Backward" "Reference Frame" Frame3d.backward $(docs 'Frame3d.backward)
    , Class.factory1 "Rightward" "Reference Frame" Frame3d.rightward $(docs 'Frame3d.rightward)
    , Class.factory1 "Leftward" "Reference Frame" Frame3d.leftward $(docs 'Frame3d.leftward)
    , Class.factory1 "Upward" "Reference Frame" Frame3d.upward $(docs 'Frame3d.upward)
    , Class.factory1 "Downward" "Reference Frame" Frame3d.downward $(docs 'Frame3d.downward)
    , Class.factory1 "From Front Plane" "Plane" Frame3d.fromFrontPlane $(docs 'Frame3d.fromFrontPlane)
    , Class.factory1 "From Back Plane" "Plane" Frame3d.fromBackPlane $(docs 'Frame3d.fromBackPlane)
    , Class.factory1 "From Right Plane" "Plane" Frame3d.fromRightPlane $(docs 'Frame3d.fromRightPlane)
    , Class.factory1 "From Left Plane" "Plane" Frame3d.fromLeftPlane $(docs 'Frame3d.fromLeftPlane)
    , Class.factory1 "From Top Plane" "Plane" Frame3d.fromTopPlane $(docs 'Frame3d.fromTopPlane)
    , Class.factory1 "From Bottom Plane" "Plane" Frame3d.fromBottomPlane $(docs 'Frame3d.fromBottomPlane)
    , Class.factory2 "Align" "Frame" "Reference Frame" Frame3d.align $(docs 'Frame3d.align)
    , Class.factory2 "Mate" "Frame" "Reference Frame" Frame3d.mate $(docs 'Frame3d.mate)
    , Class.property "Origin Point" (.originPoint) $(docs 'Frame3d.originPoint)
    , Class.property "Forward Direction" (.forwardDirection) $(docs 'Frame3d.forwardDirection)
    , Class.property "Backward Direction" (.backwardDirection) $(docs 'Frame3d.backwardDirection)
    , Class.property "Rightward Direction" (.rightwardDirection) $(docs 'Frame3d.rightwardDirection)
    , Class.property "Leftward Direction" (.leftwardDirection) $(docs 'Frame3d.leftwardDirection)
    , Class.property "Upward Direction" (.upwardDirection) $(docs 'Frame3d.upwardDirection)
    , Class.property "Downward Direction" (.downwardDirection) $(docs 'Frame3d.downwardDirection)
    , Class.property "Forward Axis" (.forwardAxis) $(docs 'Frame3d.forwardAxis)
    , Class.property "Backward Axis" (.backwardAxis) $(docs 'Frame3d.backwardAxis)
    , Class.property "Rightward Axis" (.rightwardAxis) $(docs 'Frame3d.rightwardAxis)
    , Class.property "Leftward Axis" (.leftwardAxis) $(docs 'Frame3d.leftwardAxis)
    , Class.property "Upward Axis" (.upwardAxis) $(docs 'Frame3d.upwardAxis)
    , Class.property "Downward Axis" (.downwardAxis) $(docs 'Frame3d.downwardAxis)
    , Class.property "Front Plane" (.frontPlane) $(docs 'Frame3d.frontPlane)
    , Class.property "Back Plane" (.backPlane) $(docs 'Frame3d.backPlane)
    , Class.property "Right Plane" (.rightPlane) $(docs 'Frame3d.rightPlane)
    , Class.property "Left Plane" (.leftPlane) $(docs 'Frame3d.leftPlane)
    , Class.property "Top Plane" (.topPlane) $(docs 'Frame3d.topPlane)
    , Class.property "Bottom Plane" (.bottomPlane) $(docs 'Frame3d.bottomPlane)
    , Class.property "Backward Frame" Frame3d.backward $(docs 'Frame3d.backward)
    , Class.property "Leftward Frame" Frame3d.leftward $(docs 'Frame3d.leftward)
    , Class.property "Rightward Frame" Frame3d.rightward $(docs 'Frame3d.rightward)
    , Class.property "Upward Frame" Frame3d.upward $(docs 'Frame3d.upward)
    , Class.property "Downward Frame" Frame3d.downward $(docs 'Frame3d.downward)
    , Class.member1 "Place In" "Other Frame" Frame3d.placeIn $(docs 'Frame3d.placeIn)
    , Class.member1 "Relative To" "Other Frame" Frame3d.relativeTo $(docs 'Frame3d.relativeTo)
    , Class.member0 "Inverse" Frame3d.inverse $(docs 'Frame3d.inverse)
    , Class.member1 "Move To" "Point" Frame3d.moveTo $(docs 'Frame3d.moveTo)
    , Class.member1 "Offset Forward By" "Distance" Frame3d.offsetForwardBy $(docs 'Frame3d.offsetForwardBy)
    , Class.member1 "Offset Backward By" "Distance" Frame3d.offsetBackwardBy $(docs 'Frame3d.offsetBackwardBy)
    , Class.member1 "Offset Rightward By" "Distance" Frame3d.offsetRightwardBy $(docs 'Frame3d.offsetRightwardBy)
    , Class.member1 "Offset Leftward By" "Distance" Frame3d.offsetLeftwardBy $(docs 'Frame3d.offsetLeftwardBy)
    , Class.member1 "Offset Upward By" "Distance" Frame3d.offsetUpwardBy $(docs 'Frame3d.offsetUpwardBy)
    , Class.member1 "Offset Downward By" "Distance" Frame3d.offsetDownwardBy $(docs 'Frame3d.offsetDownwardBy)
    , Class.member1 "Turn Right By" "Angle" Frame3d.turnRightBy $(docs 'Frame3d.turnRightBy)
    , Class.member1 "Turn Left By" "Angle" Frame3d.turnLeftBy $(docs 'Frame3d.turnLeftBy)
    , Class.member1 "Roll Right By" "Angle" Frame3d.rollRightBy $(docs 'Frame3d.rollRightBy)
    , Class.member1 "Roll Left By" "Angle" Frame3d.rollLeftBy $(docs 'Frame3d.rollLeftBy)
    , Class.member1 "Tilt Up By" "Angle" Frame3d.tiltUpBy $(docs 'Frame3d.tiltUpBy)
    , Class.member1 "Tilt Down By" "Angle" Frame3d.tiltDownBy $(docs 'Frame3d.tiltDownBy)
    , Class.member0 "Turn Right" Frame3d.turnRight $(docs 'Frame3d.turnRight)
    , Class.member0 "Turn Left" Frame3d.turnLeft $(docs 'Frame3d.turnLeft)
    , Class.member0 "Roll Right" Frame3d.rollRight $(docs 'Frame3d.rollRight)
    , Class.member0 "Roll Left" Frame3d.rollLeft $(docs 'Frame3d.rollLeft)
    , Class.member0 "Tilt Up" Frame3d.tiltUp $(docs 'Frame3d.tiltUp)
    , Class.member0 "Tilt Down" Frame3d.tiltDown $(docs 'Frame3d.tiltDown)
    ]
      <> rigidTransformations3d Frame3d.transformBy

vectorCurve2d :: Class
vectorCurve2d =
  Class.new @(VectorCurve2d (Space @ Unitless)) "A parametric curve defining a 2D unitless vector in terms of a parameter value." $
    [ Class.constant "Zero" (VectorCurve2d.zero @Space @Unitless) $(docs 'VectorCurve2d.zero)
    , Class.factory1 "Constant" "Value" VectorCurve2d.constant $(docs 'VectorCurve2d.constant)
    , Class.factory2 "XY" "X Component" "Y Component" VectorCurve2d.xy $(docs 'VectorCurve2d.xy)
    , Class.member1 "Evaluate" "Parameter Value" (flip VectorCurve2d.evaluate) $(docs 'VectorCurve2d.evaluate)
    ]

displacementCurve2d :: Class
displacementCurve2d =
  Class.new @(VectorCurve2d (Space @ Meters)) "A parametric curve defining a 2D displacement vector in terms of a parameter value." $
    [ Class.constant "Zero" (VectorCurve2d.zero @Space @Meters) $(docs 'VectorCurve2d.zero)
    , Class.factory1 "Constant" "Value" VectorCurve2d.constant $(docs 'VectorCurve2d.constant)
    , Class.factory2 "XY" "X Component" "Y Component" VectorCurve2d.xy $(docs 'VectorCurve2d.xy)
    , Class.member1 "Evaluate" "Parameter Value" (flip VectorCurve2d.evaluate) $(docs 'VectorCurve2d.evaluate)
    ]

-- Require that a particular value type,
-- plus all the argument types used in its transformation functions,
-- all have FFI instances
type Transformation2d value space units =
  ( FFI value
  , FFI (Qty units)
  , FFI (Vector2d (space @ units))
  , FFI (Direction2d space)
  , FFI (Point2d (space @ units))
  , FFI (Axis2d (space @ units))
  )

rigidTransformations2d ::
  Transformation2d value space units =>
  (Transform2d.Rigid (space @ units) -> value -> value) ->
  List (Class.Member value)
rigidTransformations2d transformBy =
  [ Class.member1 "Translate By" "Displacement" (Transform2d.translateByImpl transformBy) "Translate by the given displacement."
  , Class.member2 "Translate In" "Direction" "Distance" (Transform2d.translateInImpl transformBy) "Translate in the given direction by the given distance."
  , Class.member2 "Translate Along" "Axis" "Distance" (Transform2d.translateAlongImpl transformBy) "Translate along the given axis by the given distance."
  , Class.member2 "Rotate Around" "Point" "Angle" (Transform2d.rotateAroundImpl transformBy) "Rotate around the given point by the given angle."
  ]

orthonormalTransformations2d ::
  Transformation2d value space units =>
  (forall tag. Transform.IsOrthonormal tag => Transform2d tag (space @ units) -> value -> value) ->
  List (Class.Member value)
orthonormalTransformations2d transformBy =
  Class.member1 "Mirror Across" "Axis" (Transform2d.mirrorAcrossImpl transformBy) "Mirror across the given axis."
    : rigidTransformations2d transformBy

uniformTransformations2d ::
  Transformation2d value space units =>
  (forall tag. Transform.IsUniform tag => Transform2d tag (space @ units) -> value -> value) ->
  List (Class.Member value)
uniformTransformations2d transformBy =
  Class.member2 "Scale About" "Point" "Scale" (Transform2d.scaleAboutImpl transformBy) "Scale uniformly about the given point by the given scaling factor."
    : orthonormalTransformations2d transformBy

affineTransformations2d ::
  Transformation2d value space units =>
  (forall tag. Transform2d tag (space @ units) -> value -> value) ->
  List (Class.Member value)
affineTransformations2d transformBy =
  Class.member2 "Scale Along" "Axis" "Scale" (Transform2d.scaleAlongImpl transformBy) "Scale (stretch) along the given axis by the given scaling factor."
    : uniformTransformations2d transformBy

rigidTransformations3d ::
  FFI value =>
  (Transform3d.Rigid (Space @ Meters) -> value -> value) ->
  List (Class.Member value)
rigidTransformations3d transformBy =
  [ Class.member1 "Translate By" "Displacement" (Transform3d.translateByImpl transformBy) "Translate by the given displacement."
  , Class.member2 "Translate In" "Direction" "Distance" (Transform3d.translateInImpl transformBy) "Translate in the given direction by the given distance."
  , Class.member2 "Translate Along" "Axis" "Distance" (Transform3d.translateAlongImpl transformBy) "Translate along the given axis by the given distance."
  , Class.member2 "Rotate Around" "Axis" "Angle" (Transform3d.rotateAroundImpl transformBy) "Rotate around the given axis by the given angle."
  ]

orthonormalTransformations3d ::
  FFI value =>
  (forall tag. Transform.IsOrthonormal tag => Transform3d tag (Space @ Meters) -> value -> value) ->
  List (Class.Member value)
orthonormalTransformations3d transformBy =
  Class.member1 "Mirror Across" "Plane" (Transform3d.mirrorAcrossImpl transformBy) "Mirror across the given plane."
    : rigidTransformations3d transformBy

uniformTransformations3d ::
  FFI value =>
  (forall tag. Transform.IsUniform tag => Transform3d tag (Space @ Meters) -> value -> value) ->
  List (Class.Member value)
uniformTransformations3d transformBy =
  Class.member2 "Scale About" "Point" "Scale" (Transform3d.scaleAboutImpl transformBy) "Scale uniformly about the given point by the given scaling factor."
    : orthonormalTransformations3d transformBy

affineTransformations3d ::
  FFI value =>
  (forall tag. Transform3d tag (Space @ Meters) -> value -> value) ->
  List (Class.Member value)
affineTransformations3d transformBy =
  Class.member2 "Scale Along" "Axis" "Scale" (Transform3d.scaleAlongImpl transformBy) "Scale (stretch) along the given axis by the given scaling factor."
    : uniformTransformations3d transformBy

curve2d :: Class
curve2d =
  Class.new @(Curve2d (Space @ Meters)) $(docs ''Curve2d) $
    [ Class.factory1 "Constant" "Point" Curve2d.constant $(docs 'Curve2d.constant)
    , Class.factory2 "XY" "X Coordinate" "Y Coordinate" Curve2d.xy $(docs 'Curve2d.xy)
    , Class.factory2 "Line" "Start Point" "End Point" Curve2d.line $(docs 'Curve2d.line)
    , Class.factoryM3 "Arc" "Start Point" "End Point" "Swept Angle" Curve2d.arc $(docs 'Curve2d.arc)
    , Class.factory4 "Polar Arc" "Center Point" "Radius" "Start Angle" "End Angle" (Class.curryT4 Curve2d.polarArc) $(docs 'Curve2d.polarArc)
    , Class.factory3 "Swept Arc" "Center Point" "Start Point" "Swept Angle" Curve2d.sweptArc $(docs 'Curve2d.sweptArc)
    , Class.factoryM4 "Corner Arc" "Corner Point" "Incoming" "Outgoing" "Radius" (Class.curry1T3 Curve2d.cornerArc) $(docs 'Curve2d.cornerArc)
    , Class.factory2 "Circle" "Center Point" "Diameter" (Class.curryT2 Curve2d.circle) $(docs 'Curve2d.circle)
    , Class.factory1 "Bezier" "Control Points" Curve2d.bezier $(docs 'Curve2d.bezier)
    , Class.factory4 "Hermite" "Start Point" "Start Derivatives" "End Point" "End Derivatives" Curve2d.hermite $(docs 'Curve2d.hermite)
    , Class.property "Start Point" (.startPoint) "The start point of the curve."
    , Class.property "End Point" (.endPoint) "The end point of the curve."
    , Class.member1 "Evaluate" "Parameter Value" (flip Curve2d.evaluate) $(docs 'Curve2d.evaluate)
    , Class.property "Derivative" (.derivative) "The derivative of the curve."
    , Class.member0 "Reverse" Curve2d.reverse $(docs 'Curve2d.reverse)
    , Class.property "X Coordinate" (.xCoordinate) $(docs 'Curve2d.xCoordinate)
    , Class.property "Y Coordinate" (.yCoordinate) $(docs 'Curve2d.yCoordinate)
    , Class.plus @(VectorCurve2d (Space @ Meters)) Self
    , Class.minus @(VectorCurve2d (Space @ Meters)) Self
    , Class.minusSelf
    , Class.minus @(Point2d (Space @ Meters)) Self
    ]
      <> affineTransformations2d Curve2d.transformBy

uvCurve :: Class
uvCurve =
  Class.new @(Curve2d (Space @ Unitless)) "A curve in UV parameter space." $
    [ Class.factory1 "Constant" "Point" Curve2d.constant $(docs 'Curve2d.constant)
    , Class.factory2 "UV" "U Coordinate" "V Coordinate" Curve2d.xy $(docs 'Curve2d.xy)
    , Class.factory2 "Line" "Start Point" "End Point" Curve2d.line $(docs 'Curve2d.line)
    , Class.factoryU3 "Arc" "Start Point" "End Point" "Swept Angle" Curve2d.arc $(docs 'Curve2d.arc)
    , Class.factory4 "Polar Arc" "Center Point" "Radius" "Start Angle" "End Angle" (Class.curryT4 Curve2d.polarArc) $(docs 'Curve2d.polarArc)
    , Class.factory2 "Circle" "Center Point" "Diameter" (Class.curryT2 Curve2d.circle) $(docs 'Curve2d.circle)
    , Class.factory3 "Swept Arc" "Center Point" "Start Point" "Swept Angle" Curve2d.sweptArc $(docs 'Curve2d.sweptArc)
    , Class.factoryU4 "Corner Arc" "Corner Point" "Incoming" "Outgoing" "Radius" (Class.curry1T3 Curve2d.cornerArc) $(docs 'Curve2d.cornerArc)
    , Class.factory1 "Bezier" "Control Points" Curve2d.bezier $(docs 'Curve2d.bezier)
    , Class.factory4 "Hermite" "Start Point" "Start Derivatives" "End Point" "End Derivatives" Curve2d.hermite $(docs 'Curve2d.hermite)
    , Class.property "Start Point" (.startPoint) "The start point of the curve."
    , Class.property "End Point" (.endPoint) "The end point of the curve."
    , Class.member1 "Evaluate" "Parameter Value" (flip Curve2d.evaluate) $(docs 'Curve2d.evaluate)
    , Class.property "Derivative" (.derivative) "The derivative of the curve."
    , Class.member0 "Reverse" Curve2d.reverse $(docs 'Curve2d.reverse)
    , Class.property "U Coordinate" (.xCoordinate) "Get the U coordinate of a UV curve as a scalar curve."
    , Class.property "V Coordinate" (.yCoordinate) "Get the V coordinate of a UV curve as a scalar curve."
    , Class.plus @(VectorCurve2d (Space @ Unitless)) Self
    , Class.minus @(VectorCurve2d (Space @ Unitless)) Self
    , Class.minusSelf
    , Class.minus @(Point2d (Space @ Unitless)) Self
    ]
      <> affineTransformations2d Curve2d.transformBy

region2dOuterLoopDocs :: Text
region2dOuterLoopDocs =
  Text.multiline
    [ "The list of curves forming the outer boundary of the region."
    , ""
    , "The curves will be in counterclockwise order around the region,"
    , "and will each be in the counterclockwise direction."
    ]

region2dInnerLoopsDocs :: Text
region2dInnerLoopsDocs =
  Text.multiline
    [ "The lists of curves (if any) forming the holes within the region."
    , ""
    , "The curves will be in clockwise order around each hole,"
    , "and each curve will be in the clockwise direction."
    ]

region2dBoundaryCurvesDocs :: Text
region2dBoundaryCurvesDocs = "The list of all (outer and inner) boundary curves of a region."

region2d :: Class
region2d =
  Class.new @(Region2d (Space @ Meters)) $(docs ''Region2d) $
    [ Class.factoryM1R "Bounded By" "Curves" Region2d.boundedBy $(docs 'Region2d.boundedBy)
    , Class.factoryM1R "Rectangle" "Bounding Box" Region2d.rectangle $(docs 'Region2d.rectangle)
    , Class.factoryM2R "Circle" "Center Point" "Diameter" (Class.curryT2 Region2d.circle) $(docs 'Region2d.circle)
    , Class.property "Outer Loop" (.outerLoop) region2dOuterLoopDocs
    , Class.property "Inner Loops" (.innerLoops) region2dInnerLoopsDocs
    , Class.property "Boundary Curves" (.boundaryCurves) region2dBoundaryCurvesDocs
    , Class.factoryM1R "Polygon" "Points" (Region2d.polygon @(Point2d (Space @ Meters))) $(docs 'Region2d.polygon)
    , Class.factoryM2R "Hexagon" "Center Point" "Height" (Class.curryT2 Region2d.hexagon) $(docs 'Region2d.hexagon)
    , Class.factoryM3R "Inscribed Polygon" "Num Sides" "Center Point" "Diameter" (Class.curry1T2 Region2d.inscribedPolygon) $(docs 'Region2d.inscribedPolygon)
    , Class.factoryM3R "Circumscribed Polygon" "Num Sides" "Center Point" "Diameter" (Class.curry1T2 Region2d.circumscribedPolygon) $(docs 'Region2d.circumscribedPolygon)
    , Class.memberM2 "Fillet" "Points" "Radius" Region2d.fillet $(docs 'Region2d.fillet)
    ]
      <> affineTransformations2d Region2d.transformBy

uvRegion :: Class
uvRegion =
  Class.new @(Region2d (Space @ Unitless)) "A region in UV parameter space." $
    [ Class.constant "Unit Square" Region2d.unitSquare $(docs 'Region2d.unitSquare)
    , Class.factoryU1R "Bounded By" "Curves" Region2d.boundedBy $(docs 'Region2d.boundedBy)
    , Class.factoryU1R "Rectangle" "Bounding Box" Region2d.rectangle $(docs 'Region2d.rectangle)
    , Class.factoryU2R "Circle" "Center Point" "Diameter" (Class.curryT2 Region2d.circle) $(docs 'Region2d.circle)
    , Class.property "Outer Loop" (.outerLoop) region2dOuterLoopDocs
    , Class.property "Inner Loops" (.innerLoops) region2dInnerLoopsDocs
    , Class.property "Boundary Curves" (.boundaryCurves) region2dBoundaryCurvesDocs
    ]
      <> affineTransformations2d Region2d.transformBy

body3d :: Class
body3d = do
  let writeStl path convention givenResolution body =
        Stl.writeBinary path convention Length.inMillimeters (Body3d.toMesh givenResolution body)
  let writeMitsuba path givenResolution body =
        Mitsuba.writeMeshes path [(Body3d.toMesh givenResolution body, #name "")]
  Class.new @(Body3d (Space @ Meters)) $(docs ''Body3d) $
    [ Class.factoryM3R "Extruded" "Sketch Plane" "Profile" "Distance" Body3d.extruded $(docs 'Body3d.extruded)
    , Class.factoryM4R "Revolved" "Sketch Plane" "Profile" "Axis" "Angle" Body3d.revolved $(docs 'Body3d.revolved)
    , Class.factoryM1R "Block" "Bounding Box" Body3d.block $(docs 'Body3d.block)
    , Class.factoryM2R "Sphere" "Center Point" "Diameter" (Class.curryT2 Body3d.sphere) $(docs 'Body3d.sphere)
    , Class.factoryM3R "Cylinder" "Start Point" "End Point" "Diameter" Body3d.cylinder $(docs 'Body3d.cylinder)
    , Class.factoryM3R "Cylinder Along" "Axis" "Distance" "Diameter" Body3d.cylinderAlong $(docs 'Body3d.cylinderAlong)
    , Class.member1 "Place In" "Frame" Body3d.placeIn $(docs 'Body3d.placeIn)
    , Class.member1 "Relative To" "Frame" Body3d.relativeTo $(docs 'Body3d.relativeTo)
    , Class.memberM3 "Write STL" "Path" "Convention" "Resolution" writeStl "Write a body to a binary STL file, using units of millimeters."
    , Class.memberM2 "Write Mitsuba" "Path" "Resolution" writeMitsuba "Write a body to Mitsuba 'serialized' file."
    ]

resolution :: Class
resolution =
  Class.new @(Resolution Meters) $(docs ''Resolution) $
    [ Class.factory1 "Max Error" "Error" (Resolution.maxError @Meters) $(docs 'Resolution.maxError)
    , Class.factory1 "Max Size" "Size" (Resolution.maxSize @Meters) $(docs 'Resolution.maxSize)
    , Class.constructor2 "Max Error" "Max Size" (Class.curryT2 Resolution) $(docs 'Resolution)
    ]

pbrMaterial :: Class
pbrMaterial =
  Class.new @PbrMaterial $(docs ''PbrMaterial) $
    [ Class.factory2 "Metal" "Base Color" "Roughness" PbrMaterial.metal $(docs 'PbrMaterial.metal)
    , Class.factory1 "Aluminum" "Roughness" PbrMaterial.aluminum $(docs 'PbrMaterial.aluminum)
    , Class.factory1 "Brass" "Roughness" PbrMaterial.brass $(docs 'PbrMaterial.brass)
    , Class.factory1 "Chromium" "Roughness" PbrMaterial.chromium $(docs 'PbrMaterial.chromium)
    , Class.factory1 "Copper" "Roughness" PbrMaterial.copper $(docs 'PbrMaterial.copper)
    , Class.factory1 "Gold" "Roughness" PbrMaterial.gold $(docs 'PbrMaterial.gold)
    , Class.factory1 "Iron" "Roughness" PbrMaterial.iron $(docs 'PbrMaterial.iron)
    , Class.factory1 "Nickel" "Roughness" PbrMaterial.nickel $(docs 'PbrMaterial.nickel)
    , Class.factory1 "Silver" "Roughness" PbrMaterial.silver $(docs 'PbrMaterial.silver)
    , Class.factory1 "Titanium" "Roughness" PbrMaterial.titanium $(docs 'PbrMaterial.titanium)
    , Class.factory2 "Nonmetal" "Base Color" "Roughness" PbrMaterial.nonmetal $(docs 'PbrMaterial.nonmetal)
    , Class.factory3 "Custom" "Base Color" "Metallic" "Roughness" (Class.curry1T2 PbrMaterial.custom) $(docs 'PbrMaterial.custom)
    ]

model3d :: Class
model3d =
  Class.new @(Model3d Space) $(docs ''Model3d) $
    [ Class.constant "Nothing" (Model3d.nothing @Space) $(docs 'Model3d.nothing)
    , Class.factoryM1 "Body" "Body" Model3d.body $(docs 'Model3d.body)
    , Class.factoryM2 "Body With" "Attributes" "Body" Model3d.bodyWith $(docs 'Model3d.bodyWith)
    , Class.factory1 "Group" "Children" Model3d.group $(docs 'Model3d.group)
    , Class.factory2 "Group With" "Attributes" "Children" Model3d.groupWith $(docs 'Model3d.groupWith)
    , Class.member1 "With Name" "Name" Model3d.withName $(docs 'Model3d.withName)
    , Class.member1 "With PBR Material" "Material" Model3d.withPbrMaterial $(docs 'Model3d.withPbrMaterial)
    , Class.member1 "With Opacity" "Opacity" Model3d.withOpacity $(docs 'Model3d.withOpacity)
    , Class.member1 "With Attributes" "Attributes" Model3d.withAttributes $(docs 'Model3d.withAttributes)
    , Class.nested @Model3d.Attribute $(docs ''Model3d.Attribute) []
    , Class.static1 "Name" "Name" Model3d.name $(docs 'Model3d.name)
    , Class.static1 "PBR Material" "Material" Model3d.pbrMaterial $(docs 'Model3d.pbrMaterial)
    , Class.static1 "Opacity" "Opacity" Model3d.opacity $(docs 'Model3d.opacity)
    , Class.member1 "Place In" "Frame" Model3d.placeIn $(docs 'Model3d.placeIn)
    , Class.member1 "Relative To" "Frame" Model3d.relativeTo $(docs 'Model3d.relativeTo)
    ]
      <> rigidTransformations3d Model3d.transformBy

newtype Gltf = Gltf (Model3d Space)

instance FFI Gltf where
  representation = FFI.classRepresentation "Gltf"

gltf :: Class
gltf = do
  let writeBinary path res (Gltf model) = Gltf.writeBinary path model res
  Class.new @Gltf "A glTF model that can be written out to a file." $
    [ Class.constructor1 "Model" Gltf "Construct a glTF model from a generic 3D model."
    , Class.member2 "Write Binary" "Path" "Resolution" writeBinary $(docs 'Gltf.writeBinary)
    ]

camera3d :: Class
camera3d =
  Class.new @(Camera3d (Space @ Meters)) $(docs ''Camera3d) $
    [ Class.factory3 "Look At" "Eye Point" "Focal Point" "Projection" (Class.curryT3 Camera3d.lookAt) $(docs 'Camera3d.lookAt)
    , Class.factory5 "Orbit" "Focal Point" "Azimuth" "Elevation" "Distance" "Projection" (Class.curryT5 Camera3d.orbit) $(docs 'Camera3d.orbit)
    , Class.nested @(Camera3d.Projection Meters) $(docs ''Camera3d.Projection) []
    , Class.static1 "Perspective" "Vertical FOV" (Camera3d.perspective @Meters) $(docs 'Camera3d.perspective)
    , Class.static1 "Orthographic" "Viewport Height" (Camera3d.orthographic @Meters) $(docs 'Camera3d.orthographic)
    ]

data Mitsuba = Mitsuba (Model3d Space) (Camera3d (Space @ Meters)) (Mitsuba.Lighting Space)

instance FFI Mitsuba where
  representation = FFI.classRepresentation "Mitsuba"

mitsuba :: Class
mitsuba = do
  let writeFiles :: Text -> Resolution Meters -> Mitsuba -> IO ()
      writeFiles path res (Mitsuba model camera lighting) =
        Mitsuba.writeFiles do
          #path path
          #model model
          #resolution res
          #camera camera
          #lighting lighting
  Class.new @Mitsuba "A Mitsuba scene that can be written out to a file." $
    [ Class.constructor3 "Model" "Camera" "Lighting" Mitsuba "Construct a Mitsuba scene from a 3D model, a camera and some lighting."
    , Class.member2 "Write Files" "Path" "Resolution" writeFiles $(docs 'Mitsuba.writeFiles)
    , Class.nested @(Mitsuba.Lighting Space) $(docs ''Mitsuba.Lighting) []
    , Class.static2 "Environment Map" "Frame" "Image" (Mitsuba.environmentMap @Space) $(docs 'Mitsuba.environmentMap)
    ]

spurGear :: Class
spurGear =
  Class.new @SpurGear $(docs ''SpurGear) $
    [ Class.factory2 "Metric" "Num Teeth" "Module" (Class.curryT2 SpurGear.metric) $(docs 'SpurGear.metric)
    , Class.property "Num Teeth" (.numTeeth) "The number of teeth of a gear."
    , Class.property "Module" (.module_) "The module of a gear."
    , Class.property "Pitch Diameter" (.pitchDiameter) "The pitch diameter of a gear."
    , Class.property "Outer Diameter" (.outerDiameter) "The outer diameter of a gear."
    , Class.memberM0 "Profile" SpurGear.profile $(docs 'SpurGear.profile)
    ]
