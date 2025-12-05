module OpenSolid.API (classes, functions) where

import OpenSolid.API.Class (Class, Self (Self))
import OpenSolid.API.Class qualified as Class
import OpenSolid.API.Docs (docs)
import OpenSolid.API.Function (Function)
import OpenSolid.Angle (Angle)
import OpenSolid.Angle qualified as Angle
import OpenSolid.Area (Area)
import OpenSolid.Area qualified as Area
import OpenSolid.Axis2d qualified as Axis2d
import OpenSolid.Axis3d qualified as Axis3d
import OpenSolid.Body3d qualified as Body3d
import OpenSolid.Bounds qualified as Bounds
import OpenSolid.Bounds2d qualified as Bounds2d
import OpenSolid.Bounds3d qualified as Bounds3d
import OpenSolid.Camera3d qualified as Camera3d
import OpenSolid.Color (Color)
import OpenSolid.Color qualified as Color
import OpenSolid.Convention3d (Convention3d)
import OpenSolid.Convention3d qualified as Convention3d
import OpenSolid.Curve qualified as Curve
import OpenSolid.Curve.Zero qualified as Curve.Zero
import OpenSolid.Curve2d qualified as Curve2d
import OpenSolid.Direction2d qualified as Direction2d
import OpenSolid.Direction3d qualified as Direction3d
import OpenSolid.Drawing2d qualified as Drawing2d
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Frame3d qualified as Frame3d
import OpenSolid.Gltf qualified as Gltf
import OpenSolid.Length (Length)
import OpenSolid.Length qualified as Length
import OpenSolid.List qualified as List
import OpenSolid.Mitsuba qualified as Mitsuba
import OpenSolid.Model3d qualified as Model3d
import OpenSolid.Orientation3d qualified as Orientation3d
import OpenSolid.PbrMaterial (PbrMaterial)
import OpenSolid.PbrMaterial qualified as PbrMaterial
import OpenSolid.Plane3d qualified as Plane3d
import OpenSolid.PlaneOrientation3d qualified as PlaneOrientation3d
import OpenSolid.Point2D qualified as Point2D
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Point3d qualified as Point3d
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Region2d qualified as Region2d
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
import OpenSolid.Units (SquareMeters)
import OpenSolid.UvBounds (UvBounds)
import OpenSolid.UvPoint (UvPoint)
import OpenSolid.Vector2D qualified as Vector2D
import OpenSolid.Vector2d qualified as Vector2d
import OpenSolid.Vector3d qualified as Vector3d
import OpenSolid.VectorCurve2d qualified as VectorCurve2d
import OpenSolid.World3d qualified as World3d
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
  , uvVector
  , direction2d
  , uvDirection
  , point2D
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
  , world3d
  , vectorCurve2d
  , displacementCurve2d
  , uvVectorCurve
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
functions = List.combine Class.functions classes

length :: Class
length =
  Class.new @Length $(docs ''Length) $
    [ Class.constant "Zero" Length.zero $(docs 'Length.zero)
    , Class.factory3 "Interpolate" "Start" "End" "Parameter Value" Quantity.interpolateFrom $(docs 'Quantity.interpolateFrom)
    , Class.static3 "Steps" "Start" "End" "N" (Quantity.steps @Meters) $(docs 'Quantity.steps)
    , Class.static3 "Leading" "Start" "End" "N" (Quantity.leading @Meters) $(docs 'Quantity.leading)
    , Class.static3 "Trailing" "Start" "End" "N" (Quantity.trailing @Meters) $(docs 'Quantity.trailing)
    , Class.static3 "In Between" "Start" "End" "N" (Quantity.inBetween @Meters) $(docs 'Quantity.inBetween)
    , Class.static3 "Midpoints" "Start" "End" "N" (Quantity.midpoints @Meters) $(docs 'Quantity.midpoints)
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
    , Class.absSelf Quantity.abs
    , Class.numberTimes
    , Class.plusSelf
    , Class.plus @LengthBounds Self
    , Class.plus @LengthCurve Self
    , Class.minusSelf
    , Class.minus @LengthBounds Self
    , Class.minus @LengthCurve Self
    , Class.timesNumber
    , Class.timesSelf
    , Class.times @Bounds Self
    , Class.times @LengthBounds Self
    , Class.times @Curve Self
    , Class.times @LengthCurve Self
    , Class.times @Direction2d Self
    , Class.times @Vector2d Self
    , Class.times @Displacement2d Self
    , Class.divByNumber
    , Class.divBySelf
    , Class.divBy @Bounds Self
    , Class.divBy @LengthBounds Self
    , Class.divByU (\val crv -> Curve.quotient (Curve.constant val) crv)
    , Class.divByM (\val crv -> Curve.quotient (Curve.constant val) crv)
    , Class.floorDivBySelf
    , Class.modBySelf
    ]

area :: Class
area =
  Class.new @Area $(docs ''Area) $
    [ Class.constant "Zero" Area.zero $(docs 'Area.zero)
    , Class.factory3 "Interpolate" "Start" "End" "Parameter Value" Quantity.interpolateFrom $(docs 'Quantity.interpolateFrom)
    , Class.static3 "Steps" "Start" "End" "N" (Quantity.steps @SquareMeters) $(docs 'Quantity.steps)
    , Class.static3 "Leading" "Start" "End" "N" (Quantity.leading @SquareMeters) $(docs 'Quantity.leading)
    , Class.static3 "Trailing" "Start" "End" "N" (Quantity.trailing @SquareMeters) $(docs 'Quantity.trailing)
    , Class.static3 "In Between" "Start" "End" "N" (Quantity.inBetween @SquareMeters) $(docs 'Quantity.inBetween)
    , Class.static3 "Midpoints" "Start" "End" "N" (Quantity.midpoints @SquareMeters) $(docs 'Quantity.midpoints)
    , Class.factory1 "Square Meters" "Value" Area.squareMeters $(docs 'Area.squareMeters)
    , Class.factory1 "Square Inches" "Value" Area.squareInches $(docs 'Area.squareInches)
    , Class.member0 "In Square Meters" Area.inSquareMeters $(docs 'Area.inSquareMeters)
    , Class.member0 "In Square Inches" Area.inSquareInches $(docs 'Area.inSquareInches)
    , Class.memberS0 "Is Zero" (~= Area.zero) "Check if an area is zero, within the current tolerance."
    , Class.equalityAndHash
    , Class.comparison
    , Class.negateSelf
    , Class.absSelf Quantity.abs
    , Class.numberTimes
    , Class.plusSelf
    , Class.plus @AreaBounds Self
    , Class.plus @AreaCurve Self
    , Class.minusSelf
    , Class.minus @AreaBounds Self
    , Class.minus @AreaCurve Self
    , Class.timesNumber
    , Class.times @Bounds Self
    , Class.times @Curve Self
    , Class.times @Direction2d Self
    , Class.times @Vector2d Self
    , Class.divByNumber
    , Class.divBySelf
    , Class.divBy @Length Self
    , Class.divBy @Bounds Self
    , Class.divBy @LengthBounds Self
    , Class.divBy @AreaBounds Self
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
    , Class.factory3 "Interpolate" "Start" "End" "Parameter Value" Quantity.interpolateFrom $(docs 'Quantity.interpolateFrom)
    , Class.static3 "Steps" "Start" "End" "N" (Quantity.steps @Radians) $(docs 'Quantity.steps)
    , Class.static3 "Leading" "Start" "End" "N" (Quantity.leading @Radians) $(docs 'Quantity.leading)
    , Class.static3 "Trailing" "Start" "End" "N" (Quantity.trailing @Radians) $(docs 'Quantity.trailing)
    , Class.static3 "In Between" "Start" "End" "N" (Quantity.inBetween @Radians) $(docs 'Quantity.inBetween)
    , Class.static3 "Midpoints" "Start" "End" "N" (Quantity.midpoints @Radians) $(docs 'Quantity.midpoints)
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
    , Class.absSelf Quantity.abs
    , Class.numberTimes
    , Class.plusSelf
    , Class.plus @AngleBounds Self
    , Class.plus @AngleCurve Self
    , Class.minusSelf
    , Class.minus @AngleBounds Self
    , Class.minus @AngleCurve Self
    , Class.timesNumber
    , Class.times @Bounds Self
    , Class.times @Curve Self
    , Class.divByNumber
    , Class.divBySelf
    , Class.divBy @Bounds Self
    , Class.divBy @AngleBounds Self
    , Class.divByU (\val crv -> Curve.quotient (Curve.constant val) crv)
    , Class.divByR (\val crv -> Curve.quotient (Curve.constant val) crv)
    , Class.floorDivBySelf
    , Class.modBySelf
    ]

type Bounds = Bounds.Bounds Unitless

bounds :: Class
bounds =
  Class.new @Bounds "A range of unitless values, with a lower bound and upper bound." $
    [ Class.constant "Unit Interval" Bounds.unitInterval $(docs 'Bounds.unitInterval)
    , Class.constructor2 "First Value" "Second Value" Bounds.Bounds $(docs 'Bounds.Bounds)
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
    , Class.numberPlus
    , Class.numberMinus
    , Class.numberTimes
    , Class.numberDivBy
    , Class.plusNumber
    , Class.plusSelf
    , Class.minusNumber
    , Class.minusSelf
    , Class.timesNumber
    , Class.timesSelf
    , Class.times @Length Self
    , Class.times @Area Self
    , Class.times @Angle Self
    , Class.times @LengthBounds Self
    , Class.times @AreaBounds Self
    , Class.times @AngleBounds Self
    , Class.divByNumber
    , Class.divBySelf
    ]

type LengthBounds = Bounds.Bounds Meters

lengthBounds :: Class
lengthBounds =
  Class.new @LengthBounds "A range of length values, with a lower bound and upper bound." $
    [ Class.constructor2 "First Value" "Second Value" Bounds.Bounds $(docs 'Bounds.Bounds)
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
    , Class.numberTimes
    , Class.plusSelf
    , Class.plus @Length Self
    , Class.minusSelf
    , Class.minus @Length Self
    , Class.timesNumber
    , Class.timesSelf
    , Class.times @Length Self
    , Class.times @Bounds Self
    , Class.divByNumber
    , Class.divBySelf
    , Class.divBy @Length Self
    , Class.divBy @Bounds Self
    ]

type AreaBounds = Bounds.Bounds SquareMeters

areaBounds :: Class
areaBounds =
  Class.new @AreaBounds "A range of area values, with a lower bound and upper bound." $
    [ Class.constructor2 "First Value" "Second Value" Bounds.Bounds $(docs 'Bounds.Bounds)
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
    , Class.numberTimes
    , Class.plusSelf
    , Class.plus @Area Self
    , Class.minusSelf
    , Class.minus @Area Self
    , Class.timesNumber
    , Class.times @Bounds Self
    , Class.divByNumber
    , Class.divBySelf
    , Class.divBy @Length Self
    , Class.divBy @Area Self
    , Class.divBy @Bounds Self
    , Class.divBy @LengthBounds Self
    ]

type AngleBounds = Bounds.Bounds Radians

angleBounds :: Class
angleBounds =
  Class.new @AngleBounds "A range of angle values, with a lower bound and upper bound." $
    [ Class.constructor2 "First Value" "Second Value" Bounds.Bounds $(docs 'Bounds.Bounds)
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
    , Class.numberTimes
    , Class.plusSelf
    , Class.plus @Angle Self
    , Class.minusSelf
    , Class.minus @Angle Self
    , Class.timesNumber
    , Class.times @Bounds Self
    , Class.divByNumber
    , Class.divBySelf
    , Class.divBy @Angle Self
    , Class.divBy @Bounds Self
    ]

color :: Class
color =
  Class.new @Color $(docs ''Color) $
    [ Class.factory3 "RGB1" "Red" "Green" "Blue" Color.rgb1 $(docs 'Color.rgb1)
    , Class.factory3 "RGB255" "Red" "Green" "Blue" Color.rgb255 $(docs 'Color.rgb255)
    , Class.factory3 "HSL1" "Hue" "Saturation" "Lightness" Color.hsl1 $(docs 'Color.hsl1)
    , Class.factory1 "Hex" "Hex String" Color.hex $(docs 'Color.hex)
    , Class.member0 "To RGB1" Color.toRgb1 $(docs 'Color.toRgb1)
    , Class.member0 "To RGB255" Color.toRgb255 $(docs 'Color.toRgb255)
    , Class.member0 "To HSL1" Color.toHsl1 $(docs 'Color.toHsl1)
    , Class.member0 "To Hex" Color.toHex $(docs 'Color.toHex)
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

vectorTransformations2d ::
  forall units.
  FFI (Vector2d.Vector2d units FFI.Space) =>
  List (Class.Member (Vector2d.Vector2d units FFI.Space))
vectorTransformations2d =
  [ Class.member0 "Rotate Left" Vector2d.rotateLeft $(docs 'Vector2d.rotateLeft)
  , Class.member0 "Rotate Right" Vector2d.rotateRight $(docs 'Vector2d.rotateRight)
  , Class.member1 "Rotate By" "Angle" Vector2d.rotateBy $(docs 'Vector2d.rotateBy)
  , Class.member1 "Mirror In" "Direction" Vector2d.mirrorIn $(docs 'Vector2d.mirrorIn)
  , Class.member1
      "Mirror Across"
      "Axis"
      (Vector2d.mirrorAcross :: Axis2d -> Vector2d.Vector2d units FFI.Space -> Vector2d.Vector2d units FFI.Space)
      $(docs 'Vector2d.mirrorAcross)
  ]

type Vector2d = Vector2d.Vector2d Unitless FFI.Space

vector2d :: Class
vector2d =
  Class.new @Vector2d "A unitless vector in 2D." $
    [ Class.constant "Zero" (Vector2d.zero :: Vector2d) $(docs 'Vector2d.zero)
    , Class.factory1 "Unit" "Direction" Vector2d.unit $(docs 'Vector2d.unit)
    , Class.constructor2 "X Component" "Y Component" Vector2d.Vector2d $(docs 'Vector2d.Vector2d)
    , Class.factory1 "Y" "Y Component" Vector2d.y $(docs 'Vector2d.y)
    , Class.factory1 "X" "X Component" Vector2d.x $(docs 'Vector2d.x)
    , Class.factory2 "Polar" "Magnitude" "Angle" Vector2d.polar $(docs 'Vector2d.polar)
    , Class.property "Components" Vector2d.components $(docs 'Vector2d.components)
    , Class.property "X Component" Vector2d.xComponent $(docs 'Vector2d.xComponent)
    , Class.property "Y Component" Vector2d.yComponent $(docs 'Vector2d.yComponent)
    , Class.memberU0 "Direction" Vector2d.direction $(docs 'Vector2d.direction)
    , Class.member0 "Normalize" Vector2d.normalize $(docs 'Vector2d.normalize)
    , Class.property "Angle" Vector2d.angle $(docs 'Vector2d.angle)
    , Class.member1 "Angle To" "Other" (flip Vector2d.angleFrom) $(docs 'Vector2d.angleFrom)
    , Class.memberU0 "Is Zero" (~= Vector2d.zero) "Check if a vector is zero, within the current tolerance."
    , Class.member1 "Place On" "Plane" (Vector2d.placeOn :: Plane3d -> Vector2d -> Vector3d) $(docs 'Vector2d.placeOn)
    , Class.negateSelf
    , Class.numberTimes
    , Class.plusSelf
    , Class.minusSelf
    , Class.timesNumber
    , Class.times @Length Self
    , Class.times @Area Self
    , Class.divByNumber
    , Class.dotSelf
    , Class.dotProduct @Displacement2d Self
    , Class.dotProduct @AreaVector2d Self
    , Class.crossSelf
    , Class.crossProduct @Displacement2d Self
    , Class.crossProduct @AreaVector2d Self
    ]
      <> vectorTransformations2d

type Displacement2d = Vector2D.Vector2D FFI.Space

displacement2d :: Class
displacement2d =
  Class.new @Displacement2d "A displacement vector in 2D." $
    [ Class.constant "Zero" (Vector2D.zero :: Displacement2d) $(docs 'Vector2D.zero)
    , Class.constructor2 "X Component" "Y Component" Vector2D.Vector2D $(docs 'Vector2D.Vector2D)
    , Class.factory1 "X" "X Component" Vector2D.x $(docs 'Vector2D.x)
    , Class.factory1 "Y" "Y Component" Vector2D.y $(docs 'Vector2D.y)
    , Class.factory2 "Polar" "Magnitude" "Angle" Vector2D.polar $(docs 'Vector2D.polar)
    , Class.factory2 "Meters" "X Component" "Y Component" Vector2D.meters $(docs 'Vector2D.meters)
    , Class.factory2 "Centimeters" "X Component" "Y Component" Vector2D.centimeters $(docs 'Vector2D.centimeters)
    , Class.factory2 "Cm" "X Component" "Y Component" Vector2D.cm $(docs 'Vector2D.cm)
    , Class.factory2 "Millimeters" "X Component" "Y Component" Vector2D.millimeters $(docs 'Vector2D.millimeters)
    , Class.factory2 "Mm" "X Component" "Y Component" Vector2D.mm $(docs 'Vector2D.mm)
    , Class.factory2 "Inches" "X Component" "Y Component" Vector2D.inches $(docs 'Vector2D.inches)
    , Class.property "Components" Vector2D.components $(docs 'Vector2D.components)
    , Class.property "X Component" Vector2D.xComponent $(docs 'Vector2D.xComponent)
    , Class.property "Y Component" Vector2D.yComponent $(docs 'Vector2D.yComponent)
    , Class.memberM0 "Direction" Vector2D.direction $(docs 'Vector2D.direction)
    , Class.member0 "Normalize" Vector2D.normalize $(docs 'Vector2D.normalize)
    , Class.property "Angle" Vector2D.angle $(docs 'Vector2D.angle)
    , Class.member1 "Angle To" "Other" (flip Vector2D.angleFrom) $(docs 'Vector2D.angleFrom)
    , Class.memberM0 "Is Zero" (~= Vector2D.zero) "Check if a displacement is zero, within the current tolerance."
    , Class.member1 "Place On" "Plane" (Vector2D.placeOn :: Plane3d -> Displacement2d -> Displacement3d) $(docs 'Vector2D.placeOn)
    , Class.negateSelf
    , Class.numberTimes
    , Class.plusSelf
    , Class.minusSelf
    , Class.timesNumber
    , Class.times @Length Self
    , Class.divByNumber
    , Class.divBy @Length Self
    , Class.dotSelf
    , Class.dotProduct @Vector2d Self
    , Class.crossSelf
    , Class.crossProduct @Vector2d Self
    ]
      <> vectorTransformations2d

type AreaVector2d = Vector2d.Vector2d SquareMeters FFI.Space

areaVector2d :: Class
areaVector2d =
  Class.new @AreaVector2d "A vector in 2D with units of area." $
    [ Class.constant "Zero" (Vector2d.zero :: AreaVector2d) $(docs 'Vector2d.zero)
    , Class.constructor2 "X Component" "Y Component" Vector2d.Vector2d $(docs 'Vector2d.Vector2d)
    , Class.factory1 "X" "X Component" Vector2d.x $(docs 'Vector2d.x)
    , Class.factory1 "Y" "Y Component" Vector2d.y $(docs 'Vector2d.y)
    , Class.factory2 "Polar" "Magnitude" "Angle" Vector2d.polar $(docs 'Vector2d.polar)
    , Class.factory2 "Square Meters" "X Component" "Y Component" Vector2d.squareMeters $(docs 'Vector2d.squareMeters)
    , Class.property "Components" Vector2d.components $(docs 'Vector2d.components)
    , Class.property "X Component" Vector2d.xComponent $(docs 'Vector2d.xComponent)
    , Class.property "Y Component" Vector2d.yComponent $(docs 'Vector2d.yComponent)
    , Class.memberS0 "Direction" Vector2d.direction $(docs 'Vector2d.direction)
    , Class.member0 "Normalize" Vector2d.normalize $(docs 'Vector2d.normalize)
    , Class.property "Angle" Vector2d.angle $(docs 'Vector2d.angle)
    , Class.member1 "Angle To" "Other" (flip Vector2d.angleFrom) $(docs 'Vector2d.angleFrom)
    , Class.memberS0 "Is Zero" (~= Vector2d.zero) "Check if an area vector is zero, within the current tolerance."
    , Class.member1 "Place On" "Plane" (Vector2d.placeOn :: Plane3d -> AreaVector2d -> AreaVector3d) $(docs 'Vector2d.placeOn)
    , Class.negateSelf
    , Class.numberTimes
    , Class.plusSelf
    , Class.minusSelf
    , Class.timesNumber
    , Class.divByNumber
    , Class.divBy @Length Self
    , Class.divBy @Area Self
    , Class.dotProduct @Vector2d Self
    , Class.crossProduct @Vector2d Self
    ]
      <> vectorTransformations2d

type UvVector = Vector2d.Vector2d Unitless UvSpace

uvVector :: Class
uvVector =
  Class.new @UvVector "A vector in UV parameter space." $
    [ Class.constant "Zero" (Vector2d.zero :: UvVector) $(docs 'Vector2d.zero)
    , Class.factory1 "Unit" "Direction" Vector2d.unit $(docs 'Vector2d.unit)
    , Class.constructor2 "U Component" "V Component" Vector2d.Vector2d "Construct a vector from its U and V components."
    , Class.factory2 "Polar" "Magnitude" "Angle" Vector2d.polar $(docs 'Vector2d.polar)
    , Class.property "Components" Vector2d.components $(docs 'Vector2d.components)
    , Class.property "U Component" Vector2d.xComponent "Get the U component of a vector."
    , Class.property "V Component" Vector2d.yComponent "Get the V component of a vector."
    , Class.memberU0 "Direction" Vector2d.direction $(docs 'Vector2d.direction)
    , Class.member0 "Normalize" Vector2d.normalize $(docs 'Vector2d.normalize)
    , Class.property "Angle" Vector2d.angle $(docs 'Vector2d.angle)
    , Class.member1 "Angle To" "Other" (flip Vector2d.angleFrom) $(docs 'Vector2d.angleFrom)
    , Class.memberU0 "Is Zero" (~= Vector2d.zero) "Check if a vector is zero, within the current tolerance."
    , Class.member0 "Rotate Left" Vector2d.rotateLeft $(docs 'Vector2d.rotateLeft)
    , Class.member0 "Rotate Right" Vector2d.rotateRight $(docs 'Vector2d.rotateRight)
    , Class.member1 "Rotate By" "Angle" Vector2d.rotateBy $(docs 'Vector2d.rotateBy)
    , Class.member1 "Mirror In" "Direction" Vector2d.mirrorIn $(docs 'Vector2d.mirrorIn)
    , Class.member1 "Mirror Across" "Axis" (Vector2d.mirrorAcross :: UvAxis -> UvVector -> UvVector) $(docs 'Vector2d.mirrorAcross)
    , Class.negateSelf
    , Class.numberTimes
    , Class.plusSelf
    , Class.minusSelf
    , Class.timesNumber
    , Class.divByNumber
    , Class.dotSelf
    , Class.crossSelf
    ]

type Direction2d = Direction2d.Direction2d FFI.Space

direction2d :: Class
direction2d =
  Class.new @Direction2d $(docs ''Direction2d.Direction2d) $
    [ Class.upcast Vector2d.unit
    , Class.constant "X" (Direction2d.x :: Direction2d) $(docs 'Direction2d.x)
    , Class.constant "Y" (Direction2d.y :: Direction2d) $(docs 'Direction2d.y)
    , Class.factory1 "From Angle" "Angle" Direction2d.fromAngle $(docs 'Direction2d.fromAngle)
    , Class.factory1 "Degrees" "Value" Direction2d.degrees $(docs 'Direction2d.degrees)
    , Class.factory1 "Radians" "Value" Direction2d.radians $(docs 'Direction2d.radians)
    , Class.member0 "Rotate Left" Direction2d.rotateLeft $(docs 'Direction2d.rotateLeft)
    , Class.member0 "Rotate Right" Direction2d.rotateRight $(docs 'Direction2d.rotateRight)
    , Class.member1 "Rotate By" "Angle" Direction2d.rotateBy $(docs 'Direction2d.rotateBy)
    , Class.member1 "Mirror In" "Direction" Direction2d.mirrorIn $(docs 'Direction2d.mirrorIn)
    , Class.member1 "Mirror Across" "Axis" (Direction2d.mirrorAcross :: Axis2d -> Direction2d -> Direction2d) $(docs 'Direction2d.mirrorAcross)
    , Class.member1 "Place On" "Plane" (Direction2d.placeOn :: Plane3d -> Direction2d -> Direction3d) $(docs 'Direction2d.placeOn)
    , Class.negateSelf
    ]

type UvDirection = Direction2d.Direction2d UvSpace

uvDirection :: Class
uvDirection =
  Class.new @UvDirection "A direction in UV parameter space." $
    [ Class.upcast Vector2d.unit
    , Class.constant "U" (Direction2d.x :: UvDirection) "The U direction."
    , Class.constant "V" (Direction2d.y :: UvDirection) "The V direction."
    , Class.factory1 "From Angle" "Angle" Direction2d.fromAngle $(docs 'Direction2d.fromAngle)
    , Class.factory1 "Degrees" "Value" Direction2d.degrees $(docs 'Direction2d.degrees)
    , Class.factory1 "Radians" "Value" Direction2d.radians $(docs 'Direction2d.radians)
    , Class.member0 "Rotate Left" Direction2d.rotateLeft $(docs 'Direction2d.rotateLeft)
    , Class.member0 "Rotate Right" Direction2d.rotateRight $(docs 'Direction2d.rotateRight)
    , Class.member1 "Rotate By" "Angle" Direction2d.rotateBy $(docs 'Direction2d.rotateBy)
    , Class.member1 "Mirror In" "Direction" Direction2d.mirrorIn $(docs 'Direction2d.mirrorIn)
    , Class.member1 "Mirror Across" "Axis" (Direction2d.mirrorAcross :: UvAxis -> UvDirection -> UvDirection) $(docs 'Direction2d.mirrorAcross)
    , Class.negateSelf
    ]

type Point2D = Point2D.Point2D FFI.Space

point2D :: Class
point2D =
  Class.new @Point2D "A point in 2D, defined by its X and Y coordinates." $
    [ Class.constant "Origin" (Point2D.origin @FFI.Space) $(docs 'Point2D.origin)
    , Class.constructor2 "X Coordinate" "Y Coordinate" Point2D.Point2D $(docs 'Point2D.Point2D)
    , Class.factory1 "X" "X Coordinate" Point2D.x $(docs 'Point2D.x)
    , Class.factory1 "Y" "Y Coordinate" Point2D.y $(docs 'Point2D.y)
    , Class.factory2 "Polar" "Radius" "Angle" Point2D.polar $(docs 'Point2D.polar)
    , Class.factory2 "Meters" "X Coordinate" "Y Coordinate" Point2D.meters $(docs 'Point2D.meters)
    , Class.factory2 "Centimeters" "X Coordinate" "Y Coordinate" Point2D.centimeters $(docs 'Point2D.centimeters)
    , Class.factory2 "Cm" "X Coordinate" "Y Coordinate" Point2D.cm $(docs 'Point2D.cm)
    , Class.factory2 "Millimeters" "X Coordinate" "Y Coordinate" Point2D.millimeters $(docs 'Point2D.millimeters)
    , Class.factory2 "Mm" "X Coordinate" "Y Coordinate" Point2D.mm $(docs 'Point2D.mm)
    , Class.factory2 "Inches" "X Coordinate" "Y Coordinate" Point2D.inches $(docs 'Point2D.inches)
    , Class.property "Coordinates" Point2D.coordinates $(docs 'Point2D.coordinates)
    , Class.property "X Coordinate" Point2D.xCoordinate $(docs 'Point2D.xCoordinate)
    , Class.property "Y Coordinate" Point2D.yCoordinate $(docs 'Point2D.yCoordinate)
    , Class.member1 "Distance To" "Other" Point2D.distanceFrom $(docs 'Point2D.distanceFrom)
    , Class.member1 "Midpoint" "Other" Point2D.midpoint $(docs 'Point2D.midpoint)
    , Class.member1 "Place On" "Plane" Point2D.placeOn $(docs 'Point2D.placeOn)
    , Class.minusSelf
    , Class.minus @Displacement2d Self
    , Class.plus @Displacement2d Self
    , Class.minus @Curve2d Self
    ]
      <> affineTransformations2d Point2D.transformBy

uvPoint :: Class
uvPoint =
  Class.new @UvPoint "A point in UV parameter space." $
    [ Class.constant "Origin" (Point2d.origin :: UvPoint) "The point with coordinates (0,0)."
    , Class.constructor2 "U Coordinate" "V Coordinate" Point2d.Point2d "Construct a point from its U and V coordinates."
    , Class.property "Coordinates" Point2d.coordinates "Get the U and V coordinates of a point."
    , Class.property "U Coordinate" Point2d.xCoordinate "Get the U coordinate of a point."
    , Class.property "V Coordinate" Point2d.yCoordinate "Get the V coordinate of a point."
    , Class.member1 "Distance To" "Other" Point2d.distanceFrom $(docs 'Point2d.distanceFrom)
    , Class.member1 "Midpoint" "Other" Point2d.midpoint $(docs 'Point2d.midpoint)
    , Class.minusSelf
    , Class.minus @UvVector Self
    , Class.plus @UvVector Self
    ]

type Bounds2d = Bounds2d.Bounds2d Meters FFI.Space

bounds2d :: Class
bounds2d =
  Class.new @Bounds2d "A bounding box in 2D." $
    [ Class.constructor2 "X Coordinate" "Y Coordinate" Bounds2d.Bounds2d $(docs 'Bounds2d.Bounds2d)
    , Class.factory1 "Constant" "Point" Bounds2d.constant $(docs 'Bounds2d.constant)
    , Class.factory2 "From Corners" "First Point" "Second Point" Bounds2d.hull2 $(docs 'Bounds2d.hull2)
    , Class.factory1 "Hull" "Points" (Bounds2d.hullN @Point2D) $(docs 'Bounds2d.hullN)
    , Class.factory1 "Aggregate" "Bounds" Bounds2d.aggregateN $(docs 'Bounds2d.aggregateN)
    , Class.property "Coordinates" Bounds2d.coordinates $(docs 'Bounds2d.coordinates)
    , Class.property "X Coordinate" Bounds2d.xCoordinate $(docs 'Bounds2d.xCoordinate)
    , Class.property "Y Coordinate" Bounds2d.yCoordinate $(docs 'Bounds2d.yCoordinate)
    , Class.plus @Displacement2d Self
    , Class.minus @Displacement2d Self
    ]
      <> affineTransformations2d Bounds2d.transformBy

uvBounds :: Class
uvBounds =
  Class.new @UvBounds "A bounding box in UV parameter space." $
    [ Class.constructor2 "U Coordinate" "V Coordinate" Bounds2d.Bounds2d "Construct a bounding box from its U and V coordinate bounds."
    , Class.factory1 "Constant" "Point" Bounds2d.constant $(docs 'Bounds2d.constant)
    , Class.factory2 "From Corners" "First Point" "Second Point" Bounds2d.hull2 $(docs 'Bounds2d.hull2)
    , Class.factory1 "Hull" "Points" (Bounds2d.hullN @UvPoint) $(docs 'Bounds2d.hullN)
    , Class.factory1 "Aggregate" "Bounds" Bounds2d.aggregateN $(docs 'Bounds2d.aggregateN)
    , Class.property "Coordinates" Bounds2d.coordinates $(docs 'Bounds2d.coordinates)
    , Class.property "U Coordinate" Bounds2d.xCoordinate "Get the U coordinate bounds of a bounding box."
    , Class.property "V Coordinate" Bounds2d.yCoordinate "Get the V coordinate bounds of a bounding box."
    , Class.plus @UvVector Self
    , Class.minus @UvVector Self
    ]

type Curve = Curve.Curve Unitless

curve :: Class
curve =
  Class.new @Curve "A parametric curve definining a unitless value in terms of a parameter value." $
    [ Class.constant "Zero" (Curve.zero @Unitless) $(docs 'Curve.zero)
    , Class.constant "T" Curve.t $(docs 'Curve.t)
    , Class.factory1 "Constant" "Value" Curve.constant $(docs 'Curve.constant)
    , Class.factory2 "Line" "Start" "End" Curve.line $(docs 'Curve.line)
    , Class.property "Derivative" Curve.derivative $(docs 'Curve.derivative)
    , Class.member0 "Squared" Curve.squared $(docs 'Curve.squared)
    , Class.memberU0 "Sqrt" Curve.sqrt $(docs 'Curve.sqrt)
    , Class.member0 "Cubed" Curve.cubed $(docs 'Curve.cubed)
    , Class.member0 "Sin" (Curve.sin . (Angle.radian .*.)) $(docs 'Curve.sin)
    , Class.member0 "Cos" (Curve.cos . (Angle.radian .*.)) $(docs 'Curve.cos)
    , Class.member1 "Evaluate" "Parameter Value" (flip Curve.evaluate) $(docs 'Curve.evaluate)
    , Class.memberU0 "Zeros" Curve.zeros $(docs 'Curve.zeros)
    , Class.memberU0 "Is Zero" (~= Curve.zero) "Check if a curve is zero everywhere, within the current tolerance."
    , Class.negateSelf
    , Class.numberPlus
    , Class.numberMinus
    , Class.numberTimes
    , Class.numberDivByU (\val crv -> Curve.quotient (Curve.constant val) crv)
    , Class.plusNumber
    , Class.plusSelf
    , Class.minusNumber
    , Class.minusSelf
    , Class.timesNumber
    , Class.timesSelf
    , Class.times @Length Self
    , Class.times @Area Self
    , Class.times @Angle Self
    , Class.times @LengthCurve Self
    , Class.times @AreaCurve Self
    , Class.times @AngleCurve Self
    , Class.divByNumber
    , Class.divByU Curve.quotient
    , Class.nested @Curve.Zero "A point where a given curve is equal to zero." $
        [ Class.property "Location" (.location) "The parameter value at which the curve is zero."
        , Class.property "Order" (.order) "The order of the solution: 0 for crossing, 1 for tangent, etc."
        , Class.property "Sign" (.sign) "The sign of the solution: the sign of the curve to the right of the solution."
        ]
    ]

type AngleCurve = Curve.Curve Radians

angleCurve :: Class
angleCurve =
  Class.new @AngleCurve "A parametric curve definining an angle in terms of a parameter value." $
    [ Class.constant "Zero" (Curve.zero @Radians) $(docs 'Curve.zero)
    , Class.factory1 "Constant" "Value" Curve.constant $(docs 'Curve.constant)
    , Class.factory2 "Line" "Start" "End" Curve.line $(docs 'Curve.line)
    , Class.property "Derivative" Curve.derivative $(docs 'Curve.derivative)
    , Class.member0 "Sin" Curve.sin $(docs 'Curve.sin)
    , Class.member0 "Cos" Curve.cos $(docs 'Curve.cos)
    , Class.member1 "Evaluate" "Parameter Value" (flip Curve.evaluate) $(docs 'Curve.evaluate)
    , Class.memberR0 "Zeros" Curve.zeros $(docs 'Curve.zeros)
    , Class.memberR0 "Is Zero" (~= Curve.zero) "Check if a curve is zero everywhere, within the current tolerance."
    , Class.negateSelf
    , Class.numberTimes
    , Class.plusSelf
    , Class.plus @Angle Self
    , Class.minusSelf
    , Class.minus @Angle Self
    , Class.timesNumber
    , Class.times @Curve Self
    , Class.divByNumber
    , Class.divByR Curve.quotient
    , Class.divBy @Angle Self
    , Class.divByU Curve.quotient
    ]

type LengthCurve = Curve.Curve Meters

lengthCurve :: Class
lengthCurve =
  Class.new @LengthCurve "A parametric curve definining a length in terms of a parameter value." $
    [ Class.constant "Zero" (Curve.zero @Meters) $(docs 'Curve.zero)
    , Class.factory1 "Constant" "Value" Curve.constant $(docs 'Curve.constant)
    , Class.factory2 "Line" "Start" "End" Curve.line $(docs 'Curve.line)
    , Class.property "Derivative" Curve.derivative $(docs 'Curve.derivative)
    , Class.member0 "Squared" Curve.squared $(docs 'Curve.squared)
    , Class.member1 "Evaluate" "Parameter Value" (flip Curve.evaluate) $(docs 'Curve.evaluate)
    , Class.memberM0 "Zeros" Curve.zeros $(docs 'Curve.zeros)
    , Class.memberM0 "Is Zero" (~= Curve.zero) "Check if a curve is zero everywhere, within the current tolerance."
    , Class.negateSelf
    , Class.numberTimes
    , Class.plusSelf
    , Class.plus @Length Self
    , Class.minusSelf
    , Class.minus @Length Self
    , Class.timesNumber
    , Class.timesSelf
    , Class.times @Length Self
    , Class.times @Curve Self
    , Class.divByNumber
    , Class.divByM Curve.quotient
    , Class.divBy @Length Self
    , Class.divByU Curve.quotient
    ]

type AreaCurve = Curve.Curve SquareMeters

areaCurve :: Class
areaCurve =
  Class.new @AreaCurve "A parametric curve definining an area in terms of a parameter value." $
    [ Class.constant "Zero" (Curve.zero @SquareMeters) $(docs 'Curve.zero)
    , Class.factory1 "Constant" "Value" Curve.constant $(docs 'Curve.constant)
    , Class.factory2 "Line" "Start" "End" Curve.line $(docs 'Curve.line)
    , Class.property "Derivative" Curve.derivative $(docs 'Curve.derivative)
    , Class.memberM0 "Sqrt" Curve.sqrt $(docs 'Curve.sqrt)
    , Class.member1 "Evaluate" "Parameter Value" (flip Curve.evaluate) $(docs 'Curve.evaluate)
    , Class.memberS0 "Zeros" Curve.zeros $(docs 'Curve.zeros)
    , Class.memberS0 "Is Zero" (~= Curve.zero) "Check if a curve is zero everywhere, within the current tolerance."
    , Class.negateSelf
    , Class.numberTimes
    , Class.plusSelf
    , Class.plus @Area Self
    , Class.minusSelf
    , Class.minus @Area Self
    , Class.timesNumber
    , Class.times @Curve Self
    , Class.divByNumber
    , Class.divByS Curve.quotient
    , Class.divBy @Length Self
    , Class.divBy @Area Self
    , Class.divByU Curve.quotient
    , Class.divByM Curve.quotient
    ]

type Drawing2d = Drawing2d.Drawing2d FFI.Space

drawing2d :: Class
drawing2d =
  Class.new @Drawing2d $(docs ''Drawing2d.Drawing2d) $
    [ Class.member1 "To SVG" "View Box" Drawing2d.toSvg $(docs 'Drawing2d.toSvg)
    , Class.member2 "Write SVG" "Path" "View Box" Drawing2d.writeSvg $(docs 'Drawing2d.writeSvg)
    , Class.factory1 "Group" "Drawings" Drawing2d.group $(docs 'Drawing2d.group)
    , Class.factory2 "Group With" "Attributes" "Drawings" Drawing2d.groupWith $(docs 'Drawing2d.groupWith)
    , Class.factory1 "Polygon" "Vertices" Drawing2d.polygon $(docs 'Drawing2d.polygon)
    , Class.factory2 "Polygon With" "Attributes" "Vertices" Drawing2d.polygonWith $(docs 'Drawing2d.polygonWith)
    , Class.factory2 "Circle" "Center Point" "Diameter" Drawing2d.circle $(docs 'Drawing2d.circle)
    , Class.factory3 "Circle With" "Attributes" "Center Point" "Diameter" Drawing2d.circleWith $(docs 'Drawing2d.circleWith)
    , Class.factory2 "Curve" "Resolution" "Curve" Drawing2d.curve $(docs 'Drawing2d.curve)
    , Class.factory3 "Curve With" "Attributes" "Resolution" "Curve" Drawing2d.curveWith $(docs 'Drawing2d.curveWith)
    , Class.constant "Black Stroke" (Drawing2d.blackStroke @FFI.Space) $(docs 'Drawing2d.blackStroke)
    , Class.static1 "Stroke Color" "Color" (Drawing2d.strokeColor @FFI.Space) $(docs 'Drawing2d.strokeColor)
    , Class.constant "No Fill" (Drawing2d.noFill @FFI.Space) $(docs 'Drawing2d.noFill)
    , Class.static1 "Fill Color" "Color" (Drawing2d.fillColor @FFI.Space) $(docs 'Drawing2d.fillColor)
    , Class.nested @(Drawing2d.Attribute FFI.Space) "A drawing attribute such as fill color or stroke width." []
    ]

type Axis2d = Axis2d.Axis2d Meters FFI.Space

axis2d :: Class
axis2d =
  Class.new @Axis2d $(docs ''Axis2d.Axis2d) $
    [ Class.constructor2 "Origin Point" "Direction" Axis2d.Axis2d $(docs 'Axis2d.Axis2d)
    , Class.property "Origin Point" (.originPoint) $(docs 'Axis2d.originPoint)
    , Class.property "Direction" (.direction) $(docs 'Axis2d.direction)
    , Class.constant "X" (Axis2d.x :: Axis2d) $(docs 'Axis2d.x)
    , Class.constant "Y" (Axis2d.y :: Axis2d) $(docs 'Axis2d.y)
    , Class.member1 "Place On" "Plane" (Axis2d.placeOn :: Plane3d -> Axis2d -> Axis3d) $(docs 'Axis2d.placeOn)
    ]
      <> orthonormalTransformations2d Axis2d.transformBy

type UvAxis = Axis2d.Axis2d Unitless UvSpace

uvAxis :: Class
uvAxis =
  Class.new @UvAxis $(docs ''Axis2d.Axis2d) $
    [ Class.constructor2 "Origin Point" "Direction" Axis2d.Axis2d $(docs 'Axis2d.Axis2d)
    , Class.property "Origin Point" (.originPoint) $(docs 'Axis2d.originPoint)
    , Class.property "Direction" (.direction) $(docs 'Axis2d.direction)
    , Class.constant "U" (Axis2d.x :: UvAxis) "The U axis."
    , Class.constant "V" (Axis2d.y :: UvAxis) "The V axis."
    ]

world3d :: Class
world3d =
  Class.static "World3d" "A collection of global datums." $
    [ Class.constant "Origin Point" (World3d.originPoint :: Point3d) $(docs 'World3d.originPoint)
    , Class.constant "Forward Direction" (World3d.forwardDirection :: Direction3d) $(docs 'World3d.forwardDirection)
    , Class.constant "Backward Direction" (World3d.backwardDirection :: Direction3d) $(docs 'World3d.backwardDirection)
    , Class.constant "Leftward Direction" (World3d.leftwardDirection :: Direction3d) $(docs 'World3d.leftwardDirection)
    , Class.constant "Rightward Direction" (World3d.rightwardDirection :: Direction3d) $(docs 'World3d.rightwardDirection)
    , Class.constant "Upward Direction" (World3d.upwardDirection :: Direction3d) $(docs 'World3d.upwardDirection)
    , Class.constant "Downward Direction" (World3d.downwardDirection :: Direction3d) $(docs 'World3d.downwardDirection)
    , Class.constant "Forward Orientation" (World3d.forwardOrientation :: Orientation3d) $(docs 'World3d.forwardOrientation)
    , Class.constant "Backward Orientation" (World3d.backwardOrientation :: Orientation3d) $(docs 'World3d.backwardOrientation)
    , Class.constant "Leftward Orientation" (World3d.leftwardOrientation :: Orientation3d) $(docs 'World3d.leftwardOrientation)
    , Class.constant "Rightward Orientation" (World3d.rightwardOrientation :: Orientation3d) $(docs 'World3d.rightwardOrientation)
    , Class.constant "Upward Orientation" (World3d.upwardOrientation :: Orientation3d) $(docs 'World3d.upwardOrientation)
    , Class.constant "Downward Orientation" (World3d.downwardOrientation :: Orientation3d) $(docs 'World3d.downwardOrientation)
    , Class.constant "Frame" (World3d.frame :: Frame3d) $(docs 'World3d.frame)
    , Class.constant "Forward Axis" (World3d.forwardAxis :: Axis3d) $(docs 'World3d.forwardAxis)
    , Class.constant "Backward Axis" (World3d.backwardAxis :: Axis3d) $(docs 'World3d.backwardAxis)
    , Class.constant "Leftward Axis" (World3d.leftwardAxis :: Axis3d) $(docs 'World3d.leftwardAxis)
    , Class.constant "Rightward Axis" (World3d.rightwardAxis :: Axis3d) $(docs 'World3d.rightwardAxis)
    , Class.constant "Upward Axis" (World3d.upwardAxis :: Axis3d) $(docs 'World3d.upwardAxis)
    , Class.constant "Downward Axis" (World3d.downwardAxis :: Axis3d) $(docs 'World3d.downwardAxis)
    , Class.constant "Front Plane" (World3d.frontPlane :: Plane3d) $(docs 'World3d.frontPlane)
    , Class.constant "Back Plane" (World3d.backPlane :: Plane3d) $(docs 'World3d.backPlane)
    , Class.constant "Left Plane" (World3d.leftPlane :: Plane3d) $(docs 'World3d.leftPlane)
    , Class.constant "Right Plane" (World3d.rightPlane :: Plane3d) $(docs 'World3d.rightPlane)
    , Class.constant "Top Plane" (World3d.topPlane :: Plane3d) $(docs 'World3d.topPlane)
    , Class.constant "Bottom Plane" (World3d.bottomPlane :: Plane3d) $(docs 'World3d.bottomPlane)
    ]

convention3d :: Class
convention3d =
  Class.new @Convention3d $(docs ''Convention3d) $
    [ Class.constant "Y Up" Convention3d.yUp $(docs 'Convention3d.yUp)
    , Class.constant "Z Up" Convention3d.zUp $(docs 'Convention3d.zUp)
    ]

type Vector3d = Vector3d.Vector3d Unitless FFI.Space

vector3d :: Class
vector3d =
  Class.new @Vector3d "A unitless vector in 3D." $
    [ Class.constant "Zero" (Vector3d.zero :: Vector3d) $(docs 'Vector3d.zero)
    , Class.factory1 "Unit" "Direction" Vector3d.unit $(docs 'Vector3d.unit)
    , Class.factory2 "XYZ" "Convention" "Components" Vector3d.xyz $(docs 'Vector3d.xyz)
    , Class.factory3 "Z Up" "X Component" "Y Component" "Z Component" Vector3d.zUp $(docs 'Vector3d.zUp)
    , Class.factory3 "Y Up" "X Component" "Y Component" "Z Component" Vector3d.yUp $(docs 'Vector3d.yUp)
    , Class.member1 "Components" "Convention" Vector3d.components $(docs 'Vector3d.components)
    , Class.member0 "Z Up Components" Vector3d.zUpComponents $(docs 'Vector3d.zUpComponents)
    , Class.member0 "Y Up Components" Vector3d.yUpComponents $(docs 'Vector3d.yUpComponents)
    , Class.memberU0 "Direction" Vector3d.direction $(docs 'Vector3d.direction)
    , Class.memberU0 "Is Zero" (~= Vector3d.zero) "Check if a vector is zero, within the current tolerance."
    , Class.member2 "Rotate In" "Direction" "Angle" Vector3d.rotateIn $(docs 'Vector3d.rotateIn)
    , Class.member2 "Rotate Around" "Axis" "Angle" (Vector3d.rotateAround :: Axis3d -> Angle -> Vector3d -> Vector3d) $(docs 'Vector3d.rotateAround)
    , Class.member1 "Mirror In" "Direction" Vector3d.mirrorIn $(docs 'Vector3d.mirrorIn)
    , Class.member1 "Mirror Across" "Plane" (Vector3d.mirrorAcross :: Plane3d -> Vector3d -> Vector3d) $(docs 'Vector3d.mirrorAcross)
    , Class.member2 "Scale In" "Direction" "Scale" Vector3d.scaleIn $(docs 'Vector3d.scaleIn)
    , Class.member2 "Scale Along" "Axis" "Scale" (Vector3d.scaleAlong :: Axis3d -> Number -> Vector3d -> Vector3d) $(docs 'Vector3d.scaleAlong)
    , Class.member1 "Place In" "Frame" (Vector3d.placeIn :: Frame3d -> Vector3d -> Vector3d) $(docs 'Vector3d.placeIn)
    , Class.member1 "Relative To" "Frame" (Vector3d.relativeTo :: Frame3d -> Vector3d -> Vector3d) $(docs 'Vector3d.relativeTo)
    , Class.negateSelf
    , Class.numberTimes
    , Class.plusSelf
    , Class.minusSelf
    , Class.timesNumber
    , Class.times @Length Self
    , Class.times @Area Self
    , Class.divByNumber
    , Class.dotSelf
    , Class.dotProduct @Displacement3d Self
    , Class.dotProduct @AreaVector3d Self
    , Class.crossSelf
    , Class.crossProduct @Displacement3d Self
    , Class.crossProduct @AreaVector3d Self
    ]

type Displacement3d = Vector3d.Vector3d Meters FFI.Space

displacement3d :: Class
displacement3d =
  Class.new @Displacement3d "A displacement vector in 3D." $
    [ Class.constant "Zero" (Vector3d.zero :: Displacement3d) $(docs 'Vector3d.zero)
    , Class.factory2 "XYZ" "Convention" "Components" Vector3d.xyz $(docs 'Vector3d.xyz)
    , Class.factory3 "Z Up" "X Component" "Y Component" "Z Component" Vector3d.zUp $(docs 'Vector3d.zUp)
    , Class.factory3 "Y Up" "X Component" "Y Component" "Z Component" Vector3d.yUp $(docs 'Vector3d.yUp)
    , Class.member1 "Components" "Convention" Vector3d.components $(docs 'Vector3d.components)
    , Class.member0 "Z Up Components" Vector3d.zUpComponents $(docs 'Vector3d.zUpComponents)
    , Class.member0 "Y Up Components" Vector3d.yUpComponents $(docs 'Vector3d.yUpComponents)
    , Class.memberM0 "Direction" Vector3d.direction $(docs 'Vector3d.direction)
    , Class.memberM0 "Is Zero" (~= Vector3d.zero) "Check if a displacement is zero, within the current tolerance."
    , Class.member2 "Rotate In" "Direction" "Angle" Vector3d.rotateIn $(docs 'Vector3d.rotateIn)
    , Class.member2 "Rotate Around" "Axis" "Angle" (Vector3d.rotateAround :: Axis3d -> Angle -> Displacement3d -> Displacement3d) $(docs 'Vector3d.rotateAround)
    , Class.member1 "Mirror In" "Direction" Vector3d.mirrorIn $(docs 'Vector3d.mirrorIn)
    , Class.member1 "Mirror Across" "Plane" (Vector3d.mirrorAcross :: Plane3d -> Displacement3d -> Displacement3d) $(docs 'Vector3d.mirrorAcross)
    , Class.member2 "Scale In" "Direction" "Scale" Vector3d.scaleIn $(docs 'Vector3d.scaleIn)
    , Class.member2 "Scale Along" "Axis" "Scale" (Vector3d.scaleAlong :: Axis3d -> Number -> Displacement3d -> Displacement3d) $(docs 'Vector3d.scaleAlong)
    , Class.member1 "Place In" "Frame" (Vector3d.placeIn :: Frame3d -> Displacement3d -> Displacement3d) $(docs 'Vector3d.placeIn)
    , Class.member1 "Relative To" "Frame" (Vector3d.relativeTo :: Frame3d -> Displacement3d -> Displacement3d) $(docs 'Vector3d.relativeTo)
    , Class.negateSelf
    , Class.numberTimes
    , Class.plusSelf
    , Class.minusSelf
    , Class.timesNumber
    , Class.times @Length Self
    , Class.divByNumber
    , Class.divBy @Length Self
    , Class.dotSelf
    , Class.dotProduct @Vector3d Self
    , Class.crossSelf
    , Class.crossProduct @Vector3d Self
    ]

type AreaVector3d = Vector3d.Vector3d SquareMeters FFI.Space

areaVector3d :: Class
areaVector3d =
  Class.new @AreaVector3d "A vector in 3D with units of area." $
    [ Class.constant "Zero" (Vector3d.zero :: AreaVector3d) $(docs 'Vector3d.zero)
    , Class.factory2 "XYZ" "Convention" "Components" Vector3d.xyz $(docs 'Vector3d.xyz)
    , Class.factory3 "Z Up" "X Component" "Y Component" "Z Component" Vector3d.zUp $(docs 'Vector3d.zUp)
    , Class.factory3 "Y Up" "X Component" "Y Component" "Z Component" Vector3d.yUp $(docs 'Vector3d.yUp)
    , Class.member1 "Components" "Convention" Vector3d.components $(docs 'Vector3d.components)
    , Class.member0 "Z Up Components" Vector3d.zUpComponents $(docs 'Vector3d.zUpComponents)
    , Class.member0 "Y Up Components" Vector3d.yUpComponents $(docs 'Vector3d.yUpComponents)
    , Class.memberS0 "Direction" Vector3d.direction $(docs 'Vector3d.direction)
    , Class.memberS0 "Is Zero" (~= Vector3d.zero) "Check if an area vector is zero, within the current tolerance."
    , Class.member2 "Rotate In" "Direction" "Angle" Vector3d.rotateIn $(docs 'Vector3d.rotateIn)
    , Class.member2 "Rotate Around" "Axis" "Angle" (Vector3d.rotateAround :: Axis3d -> Angle -> AreaVector3d -> AreaVector3d) $(docs 'Vector3d.rotateAround)
    , Class.member1 "Mirror In" "Direction" Vector3d.mirrorIn $(docs 'Vector3d.mirrorIn)
    , Class.member1 "Mirror Across" "Plane" (Vector3d.mirrorAcross :: Plane3d -> AreaVector3d -> AreaVector3d) $(docs 'Vector3d.mirrorAcross)
    , Class.member2 "Scale In" "Direction" "Scale" Vector3d.scaleIn $(docs 'Vector3d.scaleIn)
    , Class.member2 "Scale Along" "Axis" "Scale" (Vector3d.scaleAlong :: Axis3d -> Number -> AreaVector3d -> AreaVector3d) $(docs 'Vector3d.scaleAlong)
    , Class.member1 "Place In" "Frame" (Vector3d.placeIn :: Frame3d -> AreaVector3d -> AreaVector3d) $(docs 'Vector3d.placeIn)
    , Class.member1 "Relative To" "Frame" (Vector3d.relativeTo :: Frame3d -> AreaVector3d -> AreaVector3d) $(docs 'Vector3d.relativeTo)
    , Class.negateSelf
    , Class.numberTimes
    , Class.plusSelf
    , Class.minusSelf
    , Class.timesNumber
    , Class.divByNumber
    , Class.divBy @Length Self
    , Class.divBy @Area Self
    , Class.dotProduct @Vector3d Self
    , Class.crossProduct @Vector3d Self
    ]

type Direction3d = Direction3d.Direction3d FFI.Space

direction3d :: Class
direction3d =
  Class.new @Direction3d $(docs ''Direction3d.Direction3d) $
    [ Class.upcast Vector3d.unit
    , Class.member0 "Perpendicular Direction" Direction3d.perpendicularDirection $(docs 'Direction3d.perpendicularDirection)
    , Class.member1 "Angle To" "Other" Direction3d.angleFrom $(docs 'Direction3d.angleFrom)
    , Class.member2 "Rotate In" "Direction" "Angle" Direction3d.rotateIn $(docs 'Direction3d.rotateIn)
    , Class.member2 "Rotate Around" "Axis" "Angle" (Direction3d.rotateAround :: Axis3d -> Angle -> Direction3d -> Direction3d) $(docs 'Direction3d.rotateAround)
    , Class.member1 "Mirror In" "Direction" Direction3d.mirrorIn $(docs 'Direction3d.mirrorIn)
    , Class.member1 "Mirror Across" "Plane" (Direction3d.mirrorAcross :: Plane3d -> Direction3d -> Direction3d) $(docs 'Direction3d.mirrorAcross)
    , Class.member1 "Place In" "Frame" (Direction3d.placeIn :: Frame3d -> Direction3d -> Direction3d) $(docs 'Direction3d.placeIn)
    , Class.member1 "Relative To" "Frame" (Direction3d.relativeTo :: Frame3d -> Direction3d -> Direction3d) $(docs 'Direction3d.relativeTo)
    , Class.negateSelf
    ]

type Point3d = Point3d.Point3d FFI.Space

point3d :: Class
point3d =
  Class.new @Point3d "A point in 3D." $
    [ Class.factory2 "Along" "Axis" "Distance" Point3d.along $(docs 'Point3d.along)
    , Class.factory2 "XYZ" "Convention" "Coordinates" Point3d.xyz $(docs 'Point3d.xyz)
    , Class.factory3 "Z Up" "X Coordinate" "Y Coordinate" "Z Coordinate" Point3d.zUp $(docs 'Point3d.zUp)
    , Class.factory3 "Y Up" "X Coordinate" "Y Coordinate" "Z Coordinate" Point3d.yUp $(docs 'Point3d.yUp)
    , Class.member1 "Coordinates" "Convention" Point3d.coordinates $(docs 'Point3d.coordinates)
    , Class.member0 "Z Up Coordinates" Point3d.zUpCoordinates $(docs 'Point3d.zUpCoordinates)
    , Class.member0 "Y Up Coordinates" Point3d.yUpCoordinates $(docs 'Point3d.yUpCoordinates)
    , Class.member1 "Distance To" "Other" Point3d.distanceFrom $(docs 'Point3d.distanceFrom)
    , Class.member1 "Midpoint" "Other" Point3d.midpoint $(docs 'Point3d.midpoint)
    , Class.member1 "Project Onto" "Plane" Point3d.projectOnto $(docs 'Point3d.projectOnto)
    , Class.member1 "Project Into" "Plane" Point3d.projectInto $(docs 'Point3d.projectInto)
    , Class.minusSelf
    , Class.minus @Displacement3d Self
    , Class.plus @Displacement3d Self
    , Class.member1 "Place In" "Frame" (Point3d.placeIn :: Frame3d -> Point3d -> Point3d) $(docs 'Point3d.placeIn)
    , Class.member1 "Relative To" "Frame" (Point3d.relativeTo :: Frame3d -> Point3d -> Point3d) $(docs 'Point3d.relativeTo)
    ]
      <> affineTransformations3d Point3d.transformBy

type Bounds3d = Bounds3d.Bounds3d FFI.Space

bounds3d :: Class
bounds3d =
  Class.new @Bounds3d $(docs ''Bounds3d.Bounds3d) $
    [ Class.factory1 "Constant" "Point" Bounds3d.constant $(docs 'Bounds3d.constant)
    , Class.factory2 "From Corners" "First Point" "Second Point" Bounds3d.hull2 $(docs 'Bounds3d.hull2)
    , Class.factory1 @(NonEmpty Point3d) "Hull" "Points" Bounds3d.hullN $(docs 'Bounds3d.hullN)
    , Class.factory1 "Aggregate" "Bounds" Bounds3d.aggregateN $(docs 'Bounds3d.aggregateN)
    , Class.member1 "Coordinates" "Convention" Bounds3d.coordinates $(docs 'Bounds3d.coordinates)
    , Class.plus @Displacement3d Self
    , Class.minus @Displacement3d Self
    ]
      <> affineTransformations3d Bounds3d.transformBy

type Axis3d = Axis3d.Axis3d FFI.Space

axis3d :: Class
axis3d =
  Class.new @Axis3d $(docs ''Axis3d.Axis3d) $
    [ Class.constructor2 "Origin Point" "Direction" Axis3d.Axis3d $(docs 'Axis3d.Axis3d)
    , Class.property "Origin Point" (.originPoint) $(docs 'Axis3d.originPoint)
    , Class.property "Direction" (.direction) $(docs 'Axis3d.direction)
    , Class.member0 "Normal Plane" (Axis3d.normalPlane :: Axis3d -> Plane3d) $(docs 'Axis3d.normalPlane)
    , Class.member1 "Move To" "Point" Axis3d.moveTo $(docs 'Axis3d.moveTo)
    , Class.member0 "Reverse" Axis3d.reverse $(docs 'Axis3d.reverse)
    , Class.member1 "Place In" "Frame" (Axis3d.placeIn :: Frame3d -> Axis3d -> Axis3d) $(docs 'Axis3d.placeIn)
    , Class.member1 "Relative To" "Frame" (Axis3d.relativeTo :: Frame3d -> Axis3d -> Axis3d) $(docs 'Axis3d.relativeTo)
    ]
      <> orthonormalTransformations3d Axis3d.transformBy

type PlaneOrientation3d = PlaneOrientation3d.PlaneOrientation3d FFI.Space

planeOrientation3d :: Class
planeOrientation3d =
  Class.new @PlaneOrientation3d $(docs ''PlaneOrientation3d.PlaneOrientation3d) $
    [ Class.factory1 "From Normal Direction" "Direction" PlaneOrientation3d.fromNormalDirection $(docs 'PlaneOrientation3d.fromNormalDirection)
    , Class.factory1 "From X Direction" "Direction" PlaneOrientation3d.fromXDirection $(docs 'PlaneOrientation3d.fromXDirection)
    , Class.factory1 "From Y Direction" "Direction" PlaneOrientation3d.fromYDirection $(docs 'PlaneOrientation3d.fromYDirection)
    , Class.property "X Direction" (.xDirection) $(docs 'PlaneOrientation3d.xDirection)
    , Class.property "Y Direction" (.yDirection) $(docs 'PlaneOrientation3d.yDirection)
    , Class.property "Normal Direction" (.normalDirection) $(docs 'PlaneOrientation3d.normalDirection)
    , Class.member1 "Place In" "Frame" (PlaneOrientation3d.placeIn :: Frame3d -> PlaneOrientation3d -> PlaneOrientation3d) $(docs 'PlaneOrientation3d.placeIn)
    , Class.member1 "Relative To" "Frame" (PlaneOrientation3d.relativeTo :: Frame3d -> PlaneOrientation3d -> PlaneOrientation3d) $(docs 'PlaneOrientation3d.relativeTo)
    ]

type Plane3d = Plane3d.Plane3d FFI.Space FFI.Space

plane3d :: Class
plane3d =
  Class.new @Plane3d $(docs ''Plane3d.Plane3d) $
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
    , Class.member1 "Place In" "Frame" (Plane3d.placeIn :: Frame3d -> Plane3d -> Plane3d) $(docs 'Plane3d.placeIn)
    , Class.member1 "Relative To" "Frame" (Plane3d.relativeTo :: Frame3d -> Plane3d -> Plane3d) $(docs 'Plane3d.relativeTo)
    ]
      <> rigidTransformations3d Plane3d.transformBy

type Orientation3d = Orientation3d.Orientation3d FFI.Space

orientation3d :: Class
orientation3d =
  Class.new @Orientation3d $(docs ''Orientation3d.Orientation3d) $
    [ Class.property "Forward Direction" (.forwardDirection) $(docs 'Orientation3d.forwardDirection)
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
    , Class.member1 "Place In" "Frame" (Orientation3d.placeIn :: Frame3d -> Orientation3d -> Orientation3d) $(docs 'Orientation3d.placeIn)
    , Class.member1 "Relative To" "Frame" (Orientation3d.relativeTo :: Frame3d -> Orientation3d -> Orientation3d) $(docs 'Orientation3d.relativeTo)
    ]

type Frame3d = Frame3d.Frame3d FFI.Space FFI.Space

frame3d :: Class
frame3d =
  Class.new @Frame3d $(docs ''Frame3d.Frame3d) $
    [ Class.factory1 "From Front Plane" "Plane" (Frame3d.fromFrontPlane :: Plane3d -> Frame3d) $(docs 'Frame3d.fromFrontPlane)
    , Class.factory1 "From Back Plane" "Plane" (Frame3d.fromBackPlane :: Plane3d -> Frame3d) $(docs 'Frame3d.fromBackPlane)
    , Class.factory1 "From Right Plane" "Plane" (Frame3d.fromRightPlane :: Plane3d -> Frame3d) $(docs 'Frame3d.fromRightPlane)
    , Class.factory1 "From Left Plane" "Plane" (Frame3d.fromLeftPlane :: Plane3d -> Frame3d) $(docs 'Frame3d.fromLeftPlane)
    , Class.factory1 "From Top Plane" "Plane" (Frame3d.fromTopPlane :: Plane3d -> Frame3d) $(docs 'Frame3d.fromTopPlane)
    , Class.factory1 "From Bottom Plane" "Plane" (Frame3d.fromBottomPlane :: Plane3d -> Frame3d) $(docs 'Frame3d.fromBottomPlane)
    , Class.factory2 "Align" "Frame" "Reference Frame" (Frame3d.align :: Frame3d -> Frame3d -> Frame3d) $(docs 'Frame3d.align)
    , Class.factory2 "Mate" "Frame" "Reference Frame" (Frame3d.mate :: Frame3d -> Frame3d -> Frame3d) $(docs 'Frame3d.mate)
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
    , Class.property "Front Plane" (Frame3d.frontPlane :: Frame3d -> Plane3d) $(docs 'Frame3d.frontPlane)
    , Class.property "Back Plane" (Frame3d.backPlane :: Frame3d -> Plane3d) $(docs 'Frame3d.backPlane)
    , Class.property "Right Plane" (Frame3d.rightPlane :: Frame3d -> Plane3d) $(docs 'Frame3d.rightPlane)
    , Class.property "Left Plane" (Frame3d.leftPlane :: Frame3d -> Plane3d) $(docs 'Frame3d.leftPlane)
    , Class.property "Top Plane" (Frame3d.topPlane :: Frame3d -> Plane3d) $(docs 'Frame3d.topPlane)
    , Class.property "Bottom Plane" (Frame3d.bottomPlane :: Frame3d -> Plane3d) $(docs 'Frame3d.bottomPlane)
    , Class.property "Backward Frame" (Frame3d.backward :: Frame3d -> Frame3d) $(docs 'Frame3d.backward)
    , Class.property "Leftward Frame" (Frame3d.leftward :: Frame3d -> Frame3d) $(docs 'Frame3d.leftward)
    , Class.property "Rightward Frame" (Frame3d.rightward :: Frame3d -> Frame3d) $(docs 'Frame3d.rightward)
    , Class.property "Upward Frame" (Frame3d.upward :: Frame3d -> Frame3d) $(docs 'Frame3d.upward)
    , Class.property "Downward Frame" (Frame3d.downward :: Frame3d -> Frame3d) $(docs 'Frame3d.downward)
    , Class.member1 "Place In" "Other Frame" (Frame3d.placeIn :: Frame3d -> Frame3d -> Frame3d) $(docs 'Frame3d.placeIn)
    , Class.member1 "Relative To" "Other Frame" (Frame3d.relativeTo :: Frame3d -> Frame3d -> Frame3d) $(docs 'Frame3d.relativeTo)
    , Class.member0 "Inverse" Frame3d.inverse $(docs 'Frame3d.inverse)
    , Class.member1 "Move To" "Point" (Frame3d.moveTo :: Point3d -> Frame3d -> Frame3d) $(docs 'Frame3d.moveTo)
    , Class.member1 "Offset Forward By" "Distance" (Frame3d.offsetForwardBy :: Length -> Frame3d -> Frame3d) $(docs 'Frame3d.offsetForwardBy)
    , Class.member1 "Offset Backward By" "Distance" (Frame3d.offsetBackwardBy :: Length -> Frame3d -> Frame3d) $(docs 'Frame3d.offsetBackwardBy)
    , Class.member1 "Offset Rightward By" "Distance" (Frame3d.offsetRightwardBy :: Length -> Frame3d -> Frame3d) $(docs 'Frame3d.offsetRightwardBy)
    , Class.member1 "Offset Leftward By" "Distance" (Frame3d.offsetLeftwardBy :: Length -> Frame3d -> Frame3d) $(docs 'Frame3d.offsetLeftwardBy)
    , Class.member1 "Offset Upward By" "Distance" (Frame3d.offsetUpwardBy :: Length -> Frame3d -> Frame3d) $(docs 'Frame3d.offsetUpwardBy)
    , Class.member1 "Offset Downward By" "Distance" (Frame3d.offsetDownwardBy :: Length -> Frame3d -> Frame3d) $(docs 'Frame3d.offsetDownwardBy)
    , Class.member1 "Turn Right By" "Angle" (Frame3d.turnRightBy :: Angle -> Frame3d -> Frame3d) $(docs 'Frame3d.turnRightBy)
    , Class.member1 "Turn Left By" "Angle" (Frame3d.turnLeftBy :: Angle -> Frame3d -> Frame3d) $(docs 'Frame3d.turnLeftBy)
    , Class.member1 "Roll Right By" "Angle" (Frame3d.rollRightBy :: Angle -> Frame3d -> Frame3d) $(docs 'Frame3d.rollRightBy)
    , Class.member1 "Roll Left By" "Angle" (Frame3d.rollLeftBy :: Angle -> Frame3d -> Frame3d) $(docs 'Frame3d.rollLeftBy)
    , Class.member1 "Tilt Up By" "Angle" (Frame3d.tiltUpBy :: Angle -> Frame3d -> Frame3d) $(docs 'Frame3d.tiltUpBy)
    , Class.member1 "Tilt Down By" "Angle" (Frame3d.tiltDownBy :: Angle -> Frame3d -> Frame3d) $(docs 'Frame3d.tiltDownBy)
    , Class.member0 "Turn Right" (Frame3d.turnRight :: Frame3d -> Frame3d) $(docs 'Frame3d.turnRight)
    , Class.member0 "Turn Left" (Frame3d.turnLeft :: Frame3d -> Frame3d) $(docs 'Frame3d.turnLeft)
    , Class.member0 "Roll Right" (Frame3d.rollRight :: Frame3d -> Frame3d) $(docs 'Frame3d.rollRight)
    , Class.member0 "Roll Left" (Frame3d.rollLeft :: Frame3d -> Frame3d) $(docs 'Frame3d.rollLeft)
    , Class.member0 "Tilt Up" (Frame3d.tiltUp :: Frame3d -> Frame3d) $(docs 'Frame3d.tiltUp)
    , Class.member0 "Tilt Down" (Frame3d.tiltDown :: Frame3d -> Frame3d) $(docs 'Frame3d.tiltDown)
    ]
      <> rigidTransformations3d Frame3d.transformBy

type VectorCurve2d = VectorCurve2d.VectorCurve2d Unitless FFI.Space

vectorCurve2d :: Class
vectorCurve2d =
  Class.new @VectorCurve2d "A parametric curve defining a 2D unitless vector in terms of a parameter value." $
    [ Class.constant "Zero" (VectorCurve2d.zero :: VectorCurve2d) $(docs 'VectorCurve2d.zero)
    , Class.factory1 "Constant" "Value" VectorCurve2d.constant $(docs 'VectorCurve2d.constant)
    , Class.factory2 "XY" "X Component" "Y Component" VectorCurve2d.xy $(docs 'VectorCurve2d.xy)
    , Class.member1 "Evaluate" "Parameter Value" (flip VectorCurve2d.evaluate) $(docs 'VectorCurve2d.evaluate)
    ]

type DisplacementCurve2d = VectorCurve2d.VectorCurve2d Meters FFI.Space

displacementCurve2d :: Class
displacementCurve2d =
  Class.new @DisplacementCurve2d "A parametric curve defining a 2D displacement vector in terms of a parameter value." $
    [ Class.constant "Zero" (VectorCurve2d.zero :: DisplacementCurve2d) $(docs 'VectorCurve2d.zero)
    , Class.factory1 "Constant" "Value" VectorCurve2d.constant $(docs 'VectorCurve2d.constant)
    , Class.factory2 "XY" "X Component" "Y Component" VectorCurve2d.xy $(docs 'VectorCurve2d.xy)
    , Class.member1 "Evaluate" "Parameter Value" (flip VectorCurve2d.evaluate) $(docs 'VectorCurve2d.evaluate)
    ]

type UvVectorCurve = VectorCurve2d.VectorCurve2d Unitless UvSpace

uvVectorCurve :: Class
uvVectorCurve =
  Class.new @UvVectorCurve "A parametric vector curve in UV parameter space." $
    [ Class.constant "Zero" (VectorCurve2d.zero :: UvVectorCurve) $(docs 'VectorCurve2d.zero)
    , Class.factory1 "Constant" "Value" VectorCurve2d.constant $(docs 'VectorCurve2d.constant)
    , Class.factory2 "UV" "U Component" "V Component" VectorCurve2d.xy "Construct a UV vector curve from its U and V components."
    , Class.member1 "Evaluate" "Parameter Value" (flip VectorCurve2d.evaluate) $(docs 'VectorCurve2d.evaluate)
    ]

rigidTransformations2d ::
  FFI value =>
  (Transform2d.Rigid Meters FFI.Space -> value -> value) ->
  List (Class.Member value)
rigidTransformations2d transformBy =
  [ Class.member1 "Translate By" "Displacement" (Transform2d.translateByImpl transformBy) "Translate by the given displacement."
  , Class.member2 "Translate In" "Direction" "Distance" (Transform2d.translateInImpl transformBy) "Translate in the given direction by the given distance."
  , Class.member2 "Translate Along" "Axis" "Distance" (Transform2d.translateAlongImpl transformBy) "Translate along the given axis by the given distance."
  , Class.member2 "Rotate Around" "Point" "Angle" (Transform2d.rotateAroundImpl transformBy) "Rotate around the given point by the given angle."
  ]

orthonormalTransformations2d ::
  FFI value =>
  (forall tag. Transform.IsOrthonormal tag => Transform2d tag Meters FFI.Space -> value -> value) ->
  List (Class.Member value)
orthonormalTransformations2d transformBy =
  Class.member1 "Mirror Across" "Axis" (Transform2d.mirrorAcrossImpl transformBy) "Mirror across the given axis."
    : rigidTransformations2d transformBy

uniformTransformations2d ::
  FFI value =>
  (forall tag. Transform.IsUniform tag => Transform2d tag Meters FFI.Space -> value -> value) ->
  List (Class.Member value)
uniformTransformations2d transformBy =
  Class.member2 "Scale About" "Point" "Scale" (Transform2d.scaleAboutImpl transformBy) "Scale uniformly about the given point by the given scaling factor."
    : orthonormalTransformations2d transformBy

affineTransformations2d ::
  FFI value =>
  (forall tag. Transform2d tag Meters FFI.Space -> value -> value) ->
  List (Class.Member value)
affineTransformations2d transformBy =
  Class.member2 "Scale Along" "Axis" "Scale" (Transform2d.scaleAlongImpl transformBy) "Scale (stretch) along the given axis by the given scaling factor."
    : uniformTransformations2d transformBy

rigidTransformations3d ::
  FFI value =>
  (Transform3d.Rigid FFI.Space -> value -> value) ->
  List (Class.Member value)
rigidTransformations3d transformBy =
  [ Class.member1 "Translate By" "Displacement" (Transform3d.translateByImpl transformBy) "Translate by the given displacement."
  , Class.member2 "Translate In" "Direction" "Distance" (Transform3d.translateInImpl transformBy) "Translate in the given direction by the given distance."
  , Class.member2 "Translate Along" "Axis" "Distance" (Transform3d.translateAlongImpl transformBy) "Translate along the given axis by the given distance."
  , Class.member2 "Rotate Around" "Axis" "Angle" (Transform3d.rotateAroundImpl transformBy) "Rotate around the given axis by the given angle."
  ]

orthonormalTransformations3d ::
  forall value.
  FFI value =>
  (forall tag. Transform.IsOrthonormal tag => Transform3d tag FFI.Space -> value -> value) ->
  List (Class.Member value)
orthonormalTransformations3d transformBy =
  Class.member1 "Mirror Across" "Plane" (Transform3d.mirrorAcrossImpl transformBy :: Plane3d -> value -> value) "Mirror across the given plane."
    : rigidTransformations3d transformBy

uniformTransformations3d ::
  FFI value =>
  (forall tag. Transform.IsUniform tag => Transform3d tag FFI.Space -> value -> value) ->
  List (Class.Member value)
uniformTransformations3d transformBy =
  Class.member2 "Scale About" "Point" "Scale" (Transform3d.scaleAboutImpl transformBy) "Scale uniformly about the given point by the given scaling factor."
    : orthonormalTransformations3d transformBy

affineTransformations3d ::
  FFI value =>
  (forall tag. Transform3d tag FFI.Space -> value -> value) ->
  List (Class.Member value)
affineTransformations3d transformBy =
  Class.member2 "Scale Along" "Axis" "Scale" (Transform3d.scaleAlongImpl transformBy) "Scale (stretch) along the given axis by the given scaling factor."
    : uniformTransformations3d transformBy

type Curve2d = Curve2d.Curve2d Meters FFI.Space

curve2d :: Class
curve2d =
  Class.new @Curve2d $(docs ''Curve2d.Curve2d) $
    [ Class.factory1 "Constant" "Point" Curve2d.constant $(docs 'Curve2d.constant)
    , Class.factory2 "XY" "X Coordinate" "Y Coordinate" Curve2d.xy $(docs 'Curve2d.xy)
    , Class.factory2 "Line" "Start Point" "End Point" Curve2d.line $(docs 'Curve2d.line)
    , Class.factoryM3 "Arc" "Start Point" "End Point" "Swept Angle" Curve2d.arc $(docs 'Curve2d.arc)
    , Class.factory4 "Polar Arc" "Center Point" "Radius" "Start Angle" "End Angle" Curve2d.polarArc $(docs 'Curve2d.polarArc)
    , Class.factory3 "Swept Arc" "Center Point" "Start Point" "Swept Angle" Curve2d.sweptArc $(docs 'Curve2d.sweptArc)
    , Class.factoryM4 "Corner Arc" "Corner Point" "Incoming" "Outgoing" "Radius" Curve2d.cornerArc $(docs 'Curve2d.cornerArc)
    , Class.factory2 "Circle" "Center Point" "Diameter" Curve2d.circle $(docs 'Curve2d.circle)
    , Class.factory1 "Bezier" "Control Points" Curve2d.bezier $(docs 'Curve2d.bezier)
    , Class.factory4 "Hermite" "Start Point" "Start Derivatives" "End Point" "End Derivatives" Curve2d.hermite $(docs 'Curve2d.hermite)
    , Class.property "Start Point" (.startPoint) "The start point of the curve."
    , Class.property "End Point" (.endPoint) "The end point of the curve."
    , Class.member1 "Evaluate" "Parameter Value" (flip Curve2d.evaluate) $(docs 'Curve2d.evaluate)
    , Class.property "Derivative" (.derivative) "The derivative of the curve."
    , Class.member0 "Reverse" Curve2d.reverse $(docs 'Curve2d.reverse)
    , Class.property "X Coordinate" (.xCoordinate) $(docs 'Curve2d.xCoordinate)
    , Class.property "Y Coordinate" (.yCoordinate) $(docs 'Curve2d.yCoordinate)
    , Class.plus @DisplacementCurve2d Self
    , Class.minus @DisplacementCurve2d Self
    , Class.minusSelf
    , Class.minus @Point2D Self
    ]
      <> affineTransformations2d Curve2d.transformBy

type UvCurve = Curve2d.Curve2d Unitless UvSpace

uvCurve :: Class
uvCurve =
  Class.new @UvCurve "A curve in UV parameter space." $
    [ Class.factory1 "Constant" "Point" Curve2d.constant $(docs 'Curve2d.constant)
    , Class.factory2 "UV" "U Coordinate" "V Coordinate" Curve2d.xy $(docs 'Curve2d.xy)
    , Class.factory2 "Line" "Start Point" "End Point" Curve2d.line $(docs 'Curve2d.line)
    , Class.factoryU3 "Arc" "Start Point" "End Point" "Swept Angle" Curve2d.arc $(docs 'Curve2d.arc)
    , Class.factory4 "Polar Arc" "Center Point" "Radius" "Start Angle" "End Angle" Curve2d.polarArc $(docs 'Curve2d.polarArc)
    , Class.factory2 "Circle" "Center Point" "Diameter" Curve2d.circle $(docs 'Curve2d.circle)
    , Class.factory3 "Swept Arc" "Center Point" "Start Point" "Swept Angle" Curve2d.sweptArc $(docs 'Curve2d.sweptArc)
    , Class.factoryU4 "Corner Arc" "Corner Point" "Incoming" "Outgoing" "Radius" Curve2d.cornerArc $(docs 'Curve2d.cornerArc)
    , Class.factory1 "Bezier" "Control Points" Curve2d.bezier $(docs 'Curve2d.bezier)
    , Class.factory4 "Hermite" "Start Point" "Start Derivatives" "End Point" "End Derivatives" Curve2d.hermite $(docs 'Curve2d.hermite)
    , Class.property "Start Point" (.startPoint) "The start point of the curve."
    , Class.property "End Point" (.endPoint) "The end point of the curve."
    , Class.member1 "Evaluate" "Parameter Value" (flip Curve2d.evaluate) $(docs 'Curve2d.evaluate)
    , Class.property "Derivative" (.derivative) "The derivative of the curve."
    , Class.member0 "Reverse" Curve2d.reverse $(docs 'Curve2d.reverse)
    , Class.property "U Coordinate" (.xCoordinate) "Get the U coordinate of a UV curve as a scalar curve."
    , Class.property "V Coordinate" (.yCoordinate) "Get the V coordinate of a UV curve as a scalar curve."
    , Class.plus @UvVectorCurve Self
    , Class.minus @UvVectorCurve Self
    , Class.minusSelf
    , Class.minus @UvPoint Self
    ]

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

type Region2d = Region2d.Region2d Meters FFI.Space

region2d :: Class
region2d =
  Class.new @Region2d $(docs ''Region2d.Region2d) $
    [ Class.factoryM1R "Bounded By" "Curves" Region2d.boundedBy $(docs 'Region2d.boundedBy)
    , Class.factoryM1R "Rectangle" "Bounding Box" Region2d.rectangle $(docs 'Region2d.rectangle)
    , Class.factoryM2R "Circle" "Center Point" "Diameter" Region2d.circle $(docs 'Region2d.circle)
    , Class.property "Outer Loop" Region2d.outerLoop region2dOuterLoopDocs
    , Class.property "Inner Loops" Region2d.innerLoops region2dInnerLoopsDocs
    , Class.property "Boundary Curves" Region2d.boundaryCurves region2dBoundaryCurvesDocs
    , Class.factoryM1R "Polygon" "Points" (Region2d.polygon @Point2D) $(docs 'Region2d.polygon)
    , Class.factoryM2R "Hexagon" "Center Point" "Height" Region2d.hexagon $(docs 'Region2d.hexagon)
    , Class.factoryM3R "Inscribed Polygon" "Num Sides" "Center Point" "Diameter" Region2d.inscribedPolygon $(docs 'Region2d.inscribedPolygon)
    , Class.factoryM3R "Circumscribed Polygon" "Num Sides" "Center Point" "Diameter" Region2d.circumscribedPolygon $(docs 'Region2d.circumscribedPolygon)
    , Class.memberM2 "Fillet" "Points" "Radius" Region2d.fillet $(docs 'Region2d.fillet)
    ]
      <> affineTransformations2d Region2d.transformBy

type UvRegion = Region2d.Region2d Unitless UvSpace

uvRegion :: Class
uvRegion =
  Class.new @UvRegion "A region in UV parameter space." $
    [ Class.constant "Unit Square" Region2d.unitSquare $(docs 'Region2d.unitSquare)
    , Class.factoryU1R "Bounded By" "Curves" Region2d.boundedBy $(docs 'Region2d.boundedBy)
    , Class.factoryU1R "Rectangle" "Bounding Box" Region2d.rectangle $(docs 'Region2d.rectangle)
    , Class.factoryU2R "Circle" "Center Point" "Diameter" Region2d.circle $(docs 'Region2d.circle)
    , Class.property "Outer Loop" Region2d.outerLoop region2dOuterLoopDocs
    , Class.property "Inner Loops" Region2d.innerLoops region2dInnerLoopsDocs
    , Class.property "Boundary Curves" Region2d.boundaryCurves region2dBoundaryCurvesDocs
    ]

type Body3d = Body3d.Body3d FFI.Space

body3d :: Class
body3d = do
  let writeStl path convention givenResolution body =
        Stl.writeBinary path convention Length.inMillimeters (Body3d.toMesh givenResolution body)
  let writeMitsuba path givenResolution body =
        Mitsuba.writeMeshes path [(Body3d.toMesh givenResolution body, #name "")]
  Class.new @Body3d $(docs ''Body3d.Body3d) $
    [ Class.factoryM4R "Extruded" "Sketch Plane" "Profile" "Start" "End" (Body3d.extruded @FFI.Space @FFI.Space) $(docs 'Body3d.extruded)
    , Class.factoryM4R "Revolved" "Sketch Plane" "Profile" "Axis" "Angle" (Body3d.revolved @FFI.Space @FFI.Space) $(docs 'Body3d.revolved)
    , Class.factoryM1R "Block" "Bounding Box" Body3d.block $(docs 'Body3d.block)
    , Class.factoryM2R "Sphere" "Center Point" "Diameter" Body3d.sphere $(docs 'Body3d.sphere)
    , Class.factoryM3R "Cylinder" "Start Point" "End Point" "Diameter" Body3d.cylinder $(docs 'Body3d.cylinder)
    , Class.factoryM4R "Cylinder Along" "Axis" "Start" "End" "Diameter" Body3d.cylinderAlong $(docs 'Body3d.cylinderAlong)
    , Class.member1 "Place In" "Frame" (Body3d.placeIn :: Frame3d -> Body3d -> Body3d) $(docs 'Body3d.placeIn)
    , Class.member1 "Relative To" "Frame" (Body3d.relativeTo :: Frame3d -> Body3d -> Body3d) $(docs 'Body3d.relativeTo)
    , Class.memberM3 "Write STL" "Path" "Convention" "Resolution" writeStl "Write a body to a binary STL file, using units of millimeters."
    , Class.memberM2 "Write Mitsuba" "Path" "Resolution" writeMitsuba "Write a body to Mitsuba 'serialized' file."
    ]

type Resolution = Resolution.Resolution Meters

resolution :: Class
resolution =
  Class.new @Resolution $(docs ''Resolution.Resolution) $
    [ Class.factory1 "Max Error" "Error" (Resolution.maxError @Meters) $(docs 'Resolution.maxError)
    , Class.factory1 "Max Size" "Size" (Resolution.maxSize @Meters) $(docs 'Resolution.maxSize)
    , Class.constructor2 "Max Error" "Max Size" Resolution.custom $(docs 'Resolution.custom)
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
    , Class.factory3 "Custom" "Base Color" "Metallic" "Roughness" PbrMaterial.custom $(docs 'PbrMaterial.custom)
    ]

type Model3d = Model3d.Model3d FFI.Space

model3d :: Class
model3d =
  Class.new @Model3d $(docs ''Model3d.Model3d) $
    [ Class.constant "Nothing" (Model3d.nothing :: Model3d) $(docs 'Model3d.nothing)
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
    , Class.member1 "Place In" "Frame" (Model3d.placeIn :: Frame3d -> Model3d -> Model3d) $(docs 'Model3d.placeIn)
    , Class.member1 "Relative To" "Frame" (Model3d.relativeTo :: Frame3d -> Model3d -> Model3d) $(docs 'Model3d.relativeTo)
    ]
      <> rigidTransformations3d Model3d.transformBy

newtype Gltf = Gltf Model3d

instance FFI Gltf where
  representation = FFI.classRepresentation "Gltf"

gltf :: Class
gltf = do
  let writeBinary path res (Gltf model) = Gltf.writeBinary path model res
  Class.new @Gltf "A glTF model that can be written out to a file." $
    [ Class.constructor1 "Model" Gltf "Construct a glTF model from a generic 3D model."
    , Class.member2 "Write Binary" "Path" "Resolution" writeBinary $(docs 'Gltf.writeBinary)
    ]

type Camera3d = Camera3d.Camera3d FFI.Space

camera3d :: Class
camera3d =
  Class.new @Camera3d $(docs ''Camera3d.Camera3d) $
    [ Class.factory3 "Look At" "Eye Point" "Focal Point" "Projection" Camera3d.lookAt $(docs 'Camera3d.lookAt)
    , Class.factory5 "Orbit" "Focal Point" "Azimuth" "Elevation" "Distance" "Projection" Camera3d.orbit $(docs 'Camera3d.orbit)
    , Class.nested @Camera3d.Projection $(docs ''Camera3d.Projection) []
    , Class.static1 "Perspective" "Vertical FOV" Camera3d.perspective $(docs 'Camera3d.perspective)
    , Class.static1 "Orthographic" "Viewport Height" Camera3d.orthographic $(docs 'Camera3d.orthographic)
    ]

data Mitsuba = Mitsuba Model3d Camera3d (Mitsuba.Lighting FFI.Space)

instance FFI Mitsuba where
  representation = FFI.classRepresentation "Mitsuba"

mitsuba :: Class
mitsuba = do
  let writeFiles :: Text -> Resolution -> Mitsuba -> IO ()
      writeFiles path res (Mitsuba model camera lighting) =
        Mitsuba.writeFiles
          (#path path)
          (#model model)
          (#resolution res)
          (#camera camera)
          (#lighting lighting)
  Class.new @Mitsuba "A Mitsuba scene that can be written out to a file." $
    [ Class.constructor3 "Model" "Camera" "Lighting" Mitsuba "Construct a Mitsuba scene from a 3D model, a camera and some lighting."
    , Class.member2 "Write Files" "Path" "Resolution" writeFiles $(docs 'Mitsuba.writeFiles)
    , Class.nested @(Mitsuba.Lighting FFI.Space) $(docs ''Mitsuba.Lighting) []
    , Class.static2 "Environment Map" "Frame" "Image" (Mitsuba.environmentMap :: Frame3d -> Text -> Mitsuba.Lighting FFI.Space) $(docs 'Mitsuba.environmentMap)
    ]

spurGear :: Class
spurGear =
  Class.new @SpurGear $(docs ''SpurGear) $
    [ Class.factory2 "Metric" "Num Teeth" "Module" SpurGear.metric $(docs 'SpurGear.metric)
    , Class.property "Num Teeth" (.numTeeth) "The number of teeth of a gear."
    , Class.property "Module" (.module_) "The module of a gear."
    , Class.property "Pitch Diameter" (.pitchDiameter) "The pitch diameter of a gear."
    , Class.property "Outer Diameter" (.outerDiameter) "The outer diameter of a gear."
    , Class.memberM0 "Profile" (SpurGear.profile :: SpurGear -> List Curve2d) $(docs 'SpurGear.profile)
    ]
