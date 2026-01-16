module OpenSolid.API (classes, functions) where

import OpenSolid.API.Class (Class, Self (Self))
import OpenSolid.API.Class qualified as Class
import OpenSolid.API.Docs (docs)
import OpenSolid.API.Function (Function)
import OpenSolid.Angle (Angle)
import OpenSolid.Angle qualified as Angle
import OpenSolid.Area (Area)
import OpenSolid.Area qualified as Area
import OpenSolid.Axis2D qualified as Axis2D
import OpenSolid.Axis3D qualified as Axis3D
import OpenSolid.Body3D qualified as Body3D
import OpenSolid.Bounds2D qualified as Bounds2D
import OpenSolid.Bounds3D qualified as Bounds3D
import OpenSolid.Camera3D qualified as Camera3D
import OpenSolid.Circle2D qualified as Circle2D
import OpenSolid.Color (Color)
import OpenSolid.Color qualified as Color
import OpenSolid.Convention3D (Convention3D)
import OpenSolid.Convention3D qualified as Convention3D
import OpenSolid.Curve1D qualified as Curve1D
import OpenSolid.Curve1D.Zero qualified as Curve1D.Zero
import OpenSolid.Curve2D qualified as Curve2D
import OpenSolid.Direction2D qualified as Direction2D
import OpenSolid.Direction3D qualified as Direction3D
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Frame3D qualified as Frame3D
import OpenSolid.Gltf qualified as Gltf
import OpenSolid.Interval qualified as Interval
import OpenSolid.Length (Length)
import OpenSolid.Length qualified as Length
import OpenSolid.Line2D qualified as Line2D
import OpenSolid.List qualified as List
import OpenSolid.Mitsuba qualified as Mitsuba
import OpenSolid.Model3D qualified as Model3D
import OpenSolid.Orientation3D qualified as Orientation3D
import OpenSolid.PbrMaterial (PbrMaterial)
import OpenSolid.PbrMaterial qualified as PbrMaterial
import OpenSolid.Plane3D qualified as Plane3D
import OpenSolid.PlaneOrientation3D qualified as PlaneOrientation3D
import OpenSolid.Point2D qualified as Point2D
import OpenSolid.Point3D qualified as Point3D
import OpenSolid.Polygon2D qualified as Polygon2D
import OpenSolid.Polyline2D qualified as Polyline2D
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Region2D qualified as Region2D
import OpenSolid.Resolution qualified as Resolution
import OpenSolid.SpurGear (SpurGear)
import OpenSolid.SpurGear qualified as SpurGear
import OpenSolid.Stl qualified as Stl
import OpenSolid.Svg qualified as Svg
import OpenSolid.Text qualified as Text
import OpenSolid.Transform qualified as Transform
import OpenSolid.Transform2D (Transform2D)
import OpenSolid.Transform2D qualified as Transform2D
import OpenSolid.Transform3D (Transform3D)
import OpenSolid.Transform3D qualified as Transform3D
import OpenSolid.Triangle2D qualified as Triangle2D
import OpenSolid.Units (SquareMeters)
import OpenSolid.UvBounds (UvBounds)
import OpenSolid.UvPoint (UvPoint)
import OpenSolid.UvPoint qualified as UvPoint
import OpenSolid.Vector2D qualified as Vector2D
import OpenSolid.Vector3D qualified as Vector3D
import OpenSolid.VectorCurve2D qualified as VectorCurve2D
import OpenSolid.World3D qualified as World3D
import Prelude (flip)

classes :: List Class
classes =
  [ length
  , area
  , angle
  , interval
  , lengthInterval
  , areaInterval
  , angleInterval
  , color
  , vector2D
  , displacement2D
  , areaVector2D
  , uvVector
  , direction2D
  , uvDirection
  , point2D
  , uvPoint
  , bounds2D
  , uvBounds
  , line2D
  , uvLine
  , triangle2D
  , circle2D
  , uvCircle
  , polyline2D
  , uvPolyline
  , polygon2D
  , uvPolygon
  , curve
  , lengthCurve
  , areaCurve
  , angleCurve
  , svg
  , axis2D
  , uvAxis
  , convention3D
  , vector3D
  , displacement3D
  , areaVector3D
  , direction3D
  , point3D
  , bounds3D
  , axis3D
  , planeOrientation3D
  , plane3D
  , orientation3D
  , frame3D
  , world3D
  , vectorCurve2D
  , displacementCurve2D
  , uvVectorCurve
  , curve2D
  , uvCurve
  , region2D
  , uvRegion
  , body3D
  , resolution
  , pbrMaterial
  , model3D
  , gltf
  , spurGear
  , camera3D
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
    , Class.plus @LengthInterval Self
    , Class.plus @LengthCurve Self
    , Class.minusSelf
    , Class.minus @LengthInterval Self
    , Class.minus @LengthCurve Self
    , Class.timesNumber
    , Class.timesSelf
    , Class.times @Interval Self
    , Class.times @LengthInterval Self
    , Class.times @Curve Self
    , Class.times @LengthCurve Self
    , Class.times @Direction2D Self
    , Class.times @Vector2D Self
    , Class.times @Displacement2D Self
    , Class.divByNumber
    , Class.divBySelf
    , Class.divBy @Interval Self
    , Class.divBy @LengthInterval Self
    , Class.divByU (\val crv -> Curve1D.quotient (Curve1D.constant val) crv)
    , Class.divByM (\val crv -> Curve1D.quotient (Curve1D.constant val) crv)
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
    , Class.plus @AreaInterval Self
    , Class.plus @AreaCurve Self
    , Class.minusSelf
    , Class.minus @AreaInterval Self
    , Class.minus @AreaCurve Self
    , Class.timesNumber
    , Class.times @Interval Self
    , Class.times @Curve Self
    , Class.times @Direction2D Self
    , Class.times @Vector2D Self
    , Class.divByNumber
    , Class.divBySelf
    , Class.divBy @Length Self
    , Class.divBy @Interval Self
    , Class.divBy @LengthInterval Self
    , Class.divBy @AreaInterval Self
    , Class.divByU (\val crv -> Curve1D.quotient (Curve1D.constant val) crv)
    , Class.divByM (\val crv -> Curve1D.quotient (Curve1D.constant val) crv)
    , Class.divByS (\val crv -> Curve1D.quotient (Curve1D.constant val) crv)
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
    , Class.plus @AngleInterval Self
    , Class.plus @AngleCurve Self
    , Class.minusSelf
    , Class.minus @AngleInterval Self
    , Class.minus @AngleCurve Self
    , Class.timesNumber
    , Class.times @Interval Self
    , Class.times @Curve Self
    , Class.divByNumber
    , Class.divBySelf
    , Class.divBy @Interval Self
    , Class.divBy @AngleInterval Self
    , Class.divByU (\val crv -> Curve1D.quotient (Curve1D.constant val) crv)
    , Class.divByR (\val crv -> Curve1D.quotient (Curve1D.constant val) crv)
    , Class.floorDivBySelf
    , Class.modBySelf
    ]

type Interval = Interval.Interval Unitless

interval :: Class
interval =
  Class.new @Interval "A range of unitless values, with a lower bound and upper bound." $
    [ Class.constant "Unit" Interval.unit $(docs 'Interval.unit)
    , Class.constructor2 "First Value" "Second Value" Interval.Interval $(docs 'Interval.Interval)
    , Class.factory1 "Constant" "Value" Interval.constant $(docs 'Interval.constant)
    , Class.factory1 "Zero To" "Value" Interval.zeroTo $(docs 'Interval.zeroTo)
    , Class.factory1 "Symmetric" "Width" Interval.symmetric $(docs 'Interval.symmetric)
    , Class.factory1 "Hull" "Values" Interval.hullN $(docs 'Interval.hullN)
    , Class.factory1 "Aggregate" "Interval" Interval.aggregateN $(docs 'Interval.aggregateN)
    , Class.property "Endpoints" (.endpoints) $(docs 'Interval.endpoints)
    , Class.property "Lower" (.lower) $(docs 'Interval.lower)
    , Class.property "Upper" (.upper) $(docs 'Interval.upper)
    , Class.member1 "Intersection" "Other" Interval.intersection $(docs 'Interval.intersection)
    , Class.member1 "Includes" "Value" Interval.includes $(docs 'Interval.includes)
    , Class.member1 "Contains" "Other" Interval.contains $(docs 'Interval.contains)
    , Class.negateSelf
    , Class.absSelf Interval.abs
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
    , Class.times @LengthInterval Self
    , Class.times @AreaInterval Self
    , Class.times @AngleInterval Self
    , Class.divByNumber
    , Class.divBySelf
    ]

type LengthInterval = Interval.Interval Meters

lengthInterval :: Class
lengthInterval =
  Class.new @LengthInterval "A range of length values, with a lower bound and upper bound." $
    [ Class.constructor2 "First Value" "Second Value" Interval.Interval $(docs 'Interval.Interval)
    , Class.factory1 "Constant" "Value" Interval.constant $(docs 'Interval.constant)
    , Class.factory1 "Zero To" "Value" Interval.zeroTo $(docs 'Interval.zeroTo)
    , Class.factory1 "Symmetric" "Width" Interval.symmetric $(docs 'Interval.symmetric)
    , Class.factory1 "Hull" "Values" Interval.hullN $(docs 'Interval.hullN)
    , Class.factory1 "Aggregate" "Interval" Interval.aggregateN $(docs 'Interval.aggregateN)
    , Class.property "Endpoints" (.endpoints) $(docs 'Interval.endpoints)
    , Class.property "Lower" (.lower) $(docs 'Interval.lower)
    , Class.property "Upper" (.upper) $(docs 'Interval.upper)
    , Class.member1 "Intersection" "Other" Interval.intersection $(docs 'Interval.intersection)
    , Class.member1 "Includes" "Value" Interval.includes $(docs 'Interval.includes)
    , Class.member1 "Contains" "Other" Interval.contains $(docs 'Interval.contains)
    , Class.negateSelf
    , Class.absSelf Interval.abs
    , Class.numberTimes
    , Class.plusSelf
    , Class.plus @Length Self
    , Class.minusSelf
    , Class.minus @Length Self
    , Class.timesNumber
    , Class.timesSelf
    , Class.times @Length Self
    , Class.times @Interval Self
    , Class.divByNumber
    , Class.divBySelf
    , Class.divBy @Length Self
    , Class.divBy @Interval Self
    ]

type AreaInterval = Interval.Interval SquareMeters

areaInterval :: Class
areaInterval =
  Class.new @AreaInterval "A range of area values, with a lower bound and upper bound." $
    [ Class.constructor2 "First Value" "Second Value" Interval.Interval $(docs 'Interval.Interval)
    , Class.factory1 "Constant" "Value" Interval.constant $(docs 'Interval.constant)
    , Class.factory1 "Zero To" "Value" Interval.zeroTo $(docs 'Interval.zeroTo)
    , Class.factory1 "Symmetric" "Width" Interval.symmetric $(docs 'Interval.symmetric)
    , Class.factory1 "Hull" "Values" Interval.hullN $(docs 'Interval.hullN)
    , Class.factory1 "Aggregate" "Interval" Interval.aggregateN $(docs 'Interval.aggregateN)
    , Class.property "Endpoints" (.endpoints) $(docs 'Interval.endpoints)
    , Class.property "Lower" (.lower) $(docs 'Interval.lower)
    , Class.property "Upper" (.upper) $(docs 'Interval.upper)
    , Class.member1 "Intersection" "Other" Interval.intersection $(docs 'Interval.intersection)
    , Class.member1 "Includes" "Value" Interval.includes $(docs 'Interval.includes)
    , Class.member1 "Contains" "Other" Interval.contains $(docs 'Interval.contains)
    , Class.negateSelf
    , Class.absSelf Interval.abs
    , Class.numberTimes
    , Class.plusSelf
    , Class.plus @Area Self
    , Class.minusSelf
    , Class.minus @Area Self
    , Class.timesNumber
    , Class.times @Interval Self
    , Class.divByNumber
    , Class.divBySelf
    , Class.divBy @Length Self
    , Class.divBy @Area Self
    , Class.divBy @Interval Self
    , Class.divBy @LengthInterval Self
    ]

type AngleInterval = Interval.Interval Radians

angleInterval :: Class
angleInterval =
  Class.new @AngleInterval "A range of angle values, with a lower bound and upper bound." $
    [ Class.constructor2 "First Value" "Second Value" Interval.Interval $(docs 'Interval.Interval)
    , Class.factory1 "Constant" "Value" Interval.constant $(docs 'Interval.constant)
    , Class.factory1 "Zero To" "Value" Interval.zeroTo $(docs 'Interval.zeroTo)
    , Class.factory1 "Symmetric" "Width" Interval.symmetric $(docs 'Interval.symmetric)
    , Class.factory1 "Hull" "Values" Interval.hullN $(docs 'Interval.hullN)
    , Class.factory1 "Aggregate" "Interval" Interval.aggregateN $(docs 'Interval.aggregateN)
    , Class.property "Endpoints" (.endpoints) $(docs 'Interval.endpoints)
    , Class.property "Lower" (.lower) $(docs 'Interval.lower)
    , Class.property "Upper" (.upper) $(docs 'Interval.upper)
    , Class.member1 "Intersection" "Other" Interval.intersection $(docs 'Interval.intersection)
    , Class.member1 "Includes" "Value" Interval.includes $(docs 'Interval.includes)
    , Class.member1 "Contains" "Other" Interval.contains $(docs 'Interval.contains)
    , Class.negateSelf
    , Class.absSelf Interval.abs
    , Class.numberTimes
    , Class.plusSelf
    , Class.plus @Angle Self
    , Class.minusSelf
    , Class.minus @Angle Self
    , Class.timesNumber
    , Class.times @Interval Self
    , Class.divByNumber
    , Class.divBySelf
    , Class.divBy @Angle Self
    , Class.divBy @Interval Self
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

vectorTransformations2D ::
  forall units.
  FFI (Vector2D.Vector2D units FFI.Space) =>
  List (Class.Member (Vector2D.Vector2D units FFI.Space))
vectorTransformations2D =
  [ Class.member0 "Rotate Left" Vector2D.rotateLeft $(docs 'Vector2D.rotateLeft)
  , Class.member0 "Rotate Right" Vector2D.rotateRight $(docs 'Vector2D.rotateRight)
  , Class.member1 "Rotate By" "Angle" Vector2D.rotateBy $(docs 'Vector2D.rotateBy)
  , Class.member1 "Mirror In" "Direction" Vector2D.mirrorIn $(docs 'Vector2D.mirrorIn)
  , Class.member1
      "Mirror Across"
      "Axis"
      (Vector2D.mirrorAcross :: Axis2D -> Vector2D.Vector2D units FFI.Space -> Vector2D.Vector2D units FFI.Space)
      $(docs 'Vector2D.mirrorAcross)
  ]

type Vector2D = Vector2D.Vector2D Unitless FFI.Space

vector2D :: Class
vector2D =
  Class.new @Vector2D "A unitless vector in 2D." $
    [ Class.constant "Zero" (Vector2D.zero :: Vector2D) $(docs 'Vector2D.zero)
    , Class.factory1 "Unit" "Direction" Vector2D.unit $(docs 'Vector2D.unit)
    , Class.constructor2 "X Component" "Y Component" Vector2D.Vector2D $(docs 'Vector2D.Vector2D)
    , Class.factory1 "Y" "Y Component" Vector2D.y $(docs 'Vector2D.y)
    , Class.factory1 "X" "X Component" Vector2D.x $(docs 'Vector2D.x)
    , Class.factory2 "Polar" "Magnitude" "Angle" Vector2D.polar $(docs 'Vector2D.polar)
    , Class.property "Components" Vector2D.components $(docs 'Vector2D.components)
    , Class.property "X Component" Vector2D.xComponent $(docs 'Vector2D.xComponent)
    , Class.property "Y Component" Vector2D.yComponent $(docs 'Vector2D.yComponent)
    , Class.memberU0 "Direction" Vector2D.direction $(docs 'Vector2D.direction)
    , Class.memberU0 "Normalize" Vector2D.normalize $(docs 'Vector2D.normalize)
    , Class.property "Angle" Vector2D.angle $(docs 'Vector2D.angle)
    , Class.member1 "Angle To" "Other" (flip Vector2D.angleFrom) $(docs 'Vector2D.angleFrom)
    , Class.memberU0 "Is Zero" (~= Vector2D.zero) "Check if a vector is zero, within the current tolerance."
    , Class.member1 "Place On" "Plane" (Vector2D.placeOn :: Plane3D -> Vector2D -> Vector3D) $(docs 'Vector2D.placeOn)
    , Class.negateSelf
    , Class.numberTimes
    , Class.plusSelf
    , Class.minusSelf
    , Class.timesNumber
    , Class.times @Length Self
    , Class.times @Area Self
    , Class.divByNumber
    , Class.dotSelf
    , Class.dotProduct @Displacement2D Self
    , Class.dotProduct @AreaVector2D Self
    , Class.crossSelf
    , Class.crossProduct @Displacement2D Self
    , Class.crossProduct @AreaVector2D Self
    ]
      <> vectorTransformations2D

type Displacement2D = Vector2D.Vector2D Meters FFI.Space

displacement2D :: Class
displacement2D =
  Class.new @Displacement2D "A displacement vector in 2D." $
    [ Class.constant "Zero" (Vector2D.zero :: Displacement2D) $(docs 'Vector2D.zero)
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
    , Class.memberM0 "Normalize" Vector2D.normalize $(docs 'Vector2D.normalize)
    , Class.property "Angle" Vector2D.angle $(docs 'Vector2D.angle)
    , Class.member1 "Angle To" "Other" (flip Vector2D.angleFrom) $(docs 'Vector2D.angleFrom)
    , Class.memberM0 "Is Zero" (~= Vector2D.zero) "Check if a displacement is zero, within the current tolerance."
    , Class.member1 "Place On" "Plane" (Vector2D.placeOn :: Plane3D -> Displacement2D -> Displacement3D) $(docs 'Vector2D.placeOn)
    , Class.negateSelf
    , Class.numberTimes
    , Class.plusSelf
    , Class.minusSelf
    , Class.timesNumber
    , Class.times @Length Self
    , Class.divByNumber
    , Class.divBy @Length Self
    , Class.dotSelf
    , Class.dotProduct @Vector2D Self
    , Class.crossSelf
    , Class.crossProduct @Vector2D Self
    ]
      <> vectorTransformations2D

type AreaVector2D = Vector2D.Vector2D SquareMeters FFI.Space

areaVector2D :: Class
areaVector2D =
  Class.new @AreaVector2D "A vector in 2D with units of area." $
    [ Class.constant "Zero" (Vector2D.zero :: AreaVector2D) $(docs 'Vector2D.zero)
    , Class.constructor2 "X Component" "Y Component" Vector2D.Vector2D $(docs 'Vector2D.Vector2D)
    , Class.factory1 "X" "X Component" Vector2D.x $(docs 'Vector2D.x)
    , Class.factory1 "Y" "Y Component" Vector2D.y $(docs 'Vector2D.y)
    , Class.factory2 "Polar" "Magnitude" "Angle" Vector2D.polar $(docs 'Vector2D.polar)
    , Class.factory2 "Square Meters" "X Component" "Y Component" Vector2D.squareMeters $(docs 'Vector2D.squareMeters)
    , Class.property "Components" Vector2D.components $(docs 'Vector2D.components)
    , Class.property "X Component" Vector2D.xComponent $(docs 'Vector2D.xComponent)
    , Class.property "Y Component" Vector2D.yComponent $(docs 'Vector2D.yComponent)
    , Class.memberS0 "Direction" Vector2D.direction $(docs 'Vector2D.direction)
    , Class.memberS0 "Normalize" Vector2D.normalize $(docs 'Vector2D.normalize)
    , Class.property "Angle" Vector2D.angle $(docs 'Vector2D.angle)
    , Class.member1 "Angle To" "Other" (flip Vector2D.angleFrom) $(docs 'Vector2D.angleFrom)
    , Class.memberS0 "Is Zero" (~= Vector2D.zero) "Check if an area vector is zero, within the current tolerance."
    , Class.member1 "Place On" "Plane" (Vector2D.placeOn :: Plane3D -> AreaVector2D -> AreaVector3D) $(docs 'Vector2D.placeOn)
    , Class.negateSelf
    , Class.numberTimes
    , Class.plusSelf
    , Class.minusSelf
    , Class.timesNumber
    , Class.divByNumber
    , Class.divBy @Length Self
    , Class.divBy @Area Self
    , Class.dotProduct @Vector2D Self
    , Class.crossProduct @Vector2D Self
    ]
      <> vectorTransformations2D

type UvVector = Vector2D.Vector2D Unitless UvSpace

uvVector :: Class
uvVector =
  Class.new @UvVector "A vector in UV parameter space." $
    [ Class.constant "Zero" (Vector2D.zero :: UvVector) $(docs 'Vector2D.zero)
    , Class.factory1 "Unit" "Direction" Vector2D.unit $(docs 'Vector2D.unit)
    , Class.constructor2 "U Component" "V Component" Vector2D.Vector2D "Construct a vector from its U and V components."
    , Class.factory2 "Polar" "Magnitude" "Angle" Vector2D.polar $(docs 'Vector2D.polar)
    , Class.property "Components" Vector2D.components $(docs 'Vector2D.components)
    , Class.property "U Component" Vector2D.xComponent "Get the U component of a vector."
    , Class.property "V Component" Vector2D.yComponent "Get the V component of a vector."
    , Class.memberU0 "Direction" Vector2D.direction $(docs 'Vector2D.direction)
    , Class.memberU0 "Normalize" Vector2D.normalize $(docs 'Vector2D.normalize)
    , Class.property "Angle" Vector2D.angle $(docs 'Vector2D.angle)
    , Class.member1 "Angle To" "Other" (flip Vector2D.angleFrom) $(docs 'Vector2D.angleFrom)
    , Class.memberU0 "Is Zero" (~= Vector2D.zero) "Check if a vector is zero, within the current tolerance."
    , Class.member0 "Rotate Left" Vector2D.rotateLeft $(docs 'Vector2D.rotateLeft)
    , Class.member0 "Rotate Right" Vector2D.rotateRight $(docs 'Vector2D.rotateRight)
    , Class.member1 "Rotate By" "Angle" Vector2D.rotateBy $(docs 'Vector2D.rotateBy)
    , Class.member1 "Mirror In" "Direction" Vector2D.mirrorIn $(docs 'Vector2D.mirrorIn)
    , Class.member1 "Mirror Across" "Axis" (Vector2D.mirrorAcross :: UvAxis -> UvVector -> UvVector) $(docs 'Vector2D.mirrorAcross)
    , Class.negateSelf
    , Class.numberTimes
    , Class.plusSelf
    , Class.minusSelf
    , Class.timesNumber
    , Class.divByNumber
    , Class.dotSelf
    , Class.crossSelf
    ]

type Direction2D = Direction2D.Direction2D FFI.Space

direction2D :: Class
direction2D =
  Class.new @Direction2D $(docs ''Direction2D.Direction2D) $
    [ Class.upcast Vector2D.unit
    , Class.constant "X" (Direction2D.x :: Direction2D) $(docs 'Direction2D.x)
    , Class.constant "Y" (Direction2D.y :: Direction2D) $(docs 'Direction2D.y)
    , Class.factory1 "From Angle" "Angle" Direction2D.fromAngle $(docs 'Direction2D.fromAngle)
    , Class.factory1 "Degrees" "Value" Direction2D.degrees $(docs 'Direction2D.degrees)
    , Class.factory1 "Radians" "Value" Direction2D.radians $(docs 'Direction2D.radians)
    , Class.member0 "Rotate Left" Direction2D.rotateLeft $(docs 'Direction2D.rotateLeft)
    , Class.member0 "Rotate Right" Direction2D.rotateRight $(docs 'Direction2D.rotateRight)
    , Class.member1 "Rotate By" "Angle" Direction2D.rotateBy $(docs 'Direction2D.rotateBy)
    , Class.member1 "Mirror In" "Direction" Direction2D.mirrorIn $(docs 'Direction2D.mirrorIn)
    , Class.member1 "Mirror Across" "Axis" (Direction2D.mirrorAcross :: Axis2D -> Direction2D -> Direction2D) $(docs 'Direction2D.mirrorAcross)
    , Class.member1 "Place On" "Plane" (Direction2D.placeOn :: Plane3D -> Direction2D -> Direction3D) $(docs 'Direction2D.placeOn)
    , Class.negateSelf
    ]

type UvDirection = Direction2D.Direction2D UvSpace

uvDirection :: Class
uvDirection =
  Class.new @UvDirection "A direction in UV parameter space." $
    [ Class.upcast Vector2D.unit
    , Class.constant "U" (Direction2D.x :: UvDirection) "The U direction."
    , Class.constant "V" (Direction2D.y :: UvDirection) "The V direction."
    , Class.factory1 "From Angle" "Angle" Direction2D.fromAngle $(docs 'Direction2D.fromAngle)
    , Class.factory1 "Degrees" "Value" Direction2D.degrees $(docs 'Direction2D.degrees)
    , Class.factory1 "Radians" "Value" Direction2D.radians $(docs 'Direction2D.radians)
    , Class.member0 "Rotate Left" Direction2D.rotateLeft $(docs 'Direction2D.rotateLeft)
    , Class.member0 "Rotate Right" Direction2D.rotateRight $(docs 'Direction2D.rotateRight)
    , Class.member1 "Rotate By" "Angle" Direction2D.rotateBy $(docs 'Direction2D.rotateBy)
    , Class.member1 "Mirror In" "Direction" Direction2D.mirrorIn $(docs 'Direction2D.mirrorIn)
    , Class.member1 "Mirror Across" "Axis" (Direction2D.mirrorAcross :: UvAxis -> UvDirection -> UvDirection) $(docs 'Direction2D.mirrorAcross)
    , Class.negateSelf
    ]

type Point2D = Point2D.Point2D Meters FFI.Space

point2D :: Class
point2D =
  Class.new @Point2D "A point in 2D, defined by its X and Y coordinates." $
    [ Class.constant "Origin" (Point2D.origin @Meters @FFI.Space) $(docs 'Point2D.origin)
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
    , Class.minus @Displacement2D Self
    , Class.plus @Displacement2D Self
    , Class.minus @Curve2D Self
    ]
      <> affineTransformations2D Point2D.transformBy

uvPoint :: Class
uvPoint =
  Class.new @UvPoint $(docs ''UvPoint.UvPoint) $
    [ Class.constant "Origin" (Point2D.origin @Unitless @UvSpace) $(docs 'Point2D.origin)
    , Class.constructor2 "U Coordinate" "V Coordinate" Point2D.Point2D $(docs 'Point2D.Point2D)
    , Class.property "Coordinates" Point2D.coordinates $(docs 'Point2D.coordinates)
    , Class.property "U Coordinate" Point2D.xCoordinate $(docs 'Point2D.xCoordinate)
    , Class.property "V Coordinate" Point2D.yCoordinate $(docs 'Point2D.yCoordinate)
    , Class.member1 "Distance To" "Other" Point2D.distanceFrom $(docs 'Point2D.distanceFrom)
    , Class.member1 "Midpoint" "Other" Point2D.midpoint $(docs 'Point2D.midpoint)
    , Class.minusSelf
    , Class.minus @UvVector Self
    , Class.plus @UvVector Self
    ]

type Bounds2D = Bounds2D.Bounds2D Meters FFI.Space

bounds2D :: Class
bounds2D =
  Class.new @Bounds2D "A bounding box in 2D." $
    [ Class.constructor2 "X Coordinate" "Y Coordinate" Bounds2D.Bounds2D $(docs 'Bounds2D.Bounds2D)
    , Class.factory1 "Constant" "Point" Bounds2D.constant $(docs 'Bounds2D.constant)
    , Class.factory2 "From Corners" "First Point" "Second Point" Bounds2D.hull2 $(docs 'Bounds2D.hull2)
    , Class.factory1 "Hull" "Points" Bounds2D.hullN $(docs 'Bounds2D.hullN)
    , Class.factory1 "Aggregate" "Bounds" Bounds2D.aggregateN $(docs 'Bounds2D.aggregateN)
    , Class.property "Coordinates" Bounds2D.coordinates $(docs 'Bounds2D.coordinates)
    , Class.property "X Coordinate" Bounds2D.xCoordinate $(docs 'Bounds2D.xCoordinate)
    , Class.property "Y Coordinate" Bounds2D.yCoordinate $(docs 'Bounds2D.yCoordinate)
    , Class.plus @Displacement2D Self
    , Class.minus @Displacement2D Self
    ]
      <> affineTransformations2D Bounds2D.transformBy

uvBounds :: Class
uvBounds =
  Class.new @UvBounds "A bounding box in UV parameter space." $
    [ Class.constructor2 "U Coordinate" "V Coordinate" Bounds2D.Bounds2D "Construct a bounding box from its U and V coordinate bounds."
    , Class.factory1 "Constant" "Point" Bounds2D.constant $(docs 'Bounds2D.constant)
    , Class.factory2 "From Corners" "First Point" "Second Point" Bounds2D.hull2 $(docs 'Bounds2D.hull2)
    , Class.factory1 "Hull" "Points" Bounds2D.hullN $(docs 'Bounds2D.hullN)
    , Class.factory1 "Aggregate" "Bounds" Bounds2D.aggregateN $(docs 'Bounds2D.aggregateN)
    , Class.property "Coordinates" Bounds2D.coordinates $(docs 'Bounds2D.coordinates)
    , Class.property "U Coordinate" Bounds2D.xCoordinate "Get the U coordinate bounds of a bounding box."
    , Class.property "V Coordinate" Bounds2D.yCoordinate "Get the V coordinate bounds of a bounding box."
    , Class.plus @UvVector Self
    , Class.minus @UvVector Self
    ]

type Line2D = Line2D.Line2D Meters FFI.Space

line2D :: Class
line2D =
  Class.new @Line2D $(docs ''Line2D.Line2D) $
    [ Class.constructor2 "Start Point" "End Point" Line2D.Line2D $(docs 'Line2D.Line2D)
    , Class.property "Start Point" Line2D.startPoint $(docs 'Line2D.startPoint)
    , Class.property "End Point" Line2D.endPoint $(docs 'Line2D.endPoint)
    , Class.member0 "Length" Line2D.length $(docs 'Line2D.length)
    , Class.member1 "Distance To" "Point" Line2D.distanceTo $(docs 'Line2D.distanceTo)
    ]

type UvLine = Line2D.Line2D Unitless UvSpace

uvLine :: Class
uvLine =
  Class.new @UvLine $(docs ''Line2D.Line2D) $
    [ Class.constructor2 "Start Point" "End Point" Line2D.Line2D $(docs 'Line2D.Line2D)
    , Class.property "Start Point" Line2D.startPoint $(docs 'Line2D.startPoint)
    , Class.property "End Point" Line2D.endPoint $(docs 'Line2D.endPoint)
    , Class.member0 "Length" Line2D.length $(docs 'Line2D.length)
    , Class.member1 "Distance To" "Point" Line2D.distanceTo $(docs 'Line2D.distanceTo)
    ]

type Triangle2D = Triangle2D.Triangle2D Meters FFI.Space

triangle2D :: Class
triangle2D =
  Class.new @Triangle2D $(docs ''Triangle2D.Triangle2D) $
    [ Class.constructor3 "First Vertex" "Second Vertex" "Third Vertex" Triangle2D.Triangle2D $(docs 'Triangle2D.Triangle2D)
    , Class.property "Vertices" Triangle2D.vertices $(docs 'Triangle2D.vertices)
    , Class.member0 "Signed Area" Triangle2D.signedArea $(docs 'Triangle2D.signedArea)
    ]

type Circle2D = Circle2D.Circle2D Meters FFI.Space

circle2D :: Class
circle2D =
  Class.new @Circle2D $(docs ''Circle2D.Circle2D) $
    [ Class.factory2 "With Radius" "Radius" "Center Point" Circle2D.withRadius $(docs 'Circle2D.withRadius)
    , Class.factory2 "With Diameter" "Diameter" "Center Point" Circle2D.withDiameter $(docs 'Circle2D.withDiameter)
    , Class.property "Center Point" Circle2D.centerPoint $(docs 'Circle2D.centerPoint)
    , Class.property "Diameter" Circle2D.diameter $(docs 'Circle2D.diameter)
    , Class.property "Radius" Circle2D.radius $(docs 'Circle2D.radius)
    , Class.member1 "Point" "Angle" (flip Circle2D.pointOn) $(docs 'Circle2D.pointOn)
    ]

type UvCircle = Circle2D.Circle2D Unitless UvSpace

uvCircle :: Class
uvCircle =
  Class.new @UvCircle $(docs ''Circle2D.Circle2D) $
    [ Class.factory2 "With Radius" "Radius" "Center Point" Circle2D.withRadius $(docs 'Circle2D.withRadius)
    , Class.factory2 "With Diameter" "Diameter" "Center Point" Circle2D.withDiameter $(docs 'Circle2D.withDiameter)
    , Class.property "Center Point" Circle2D.centerPoint $(docs 'Circle2D.centerPoint)
    , Class.property "Diameter" Circle2D.diameter $(docs 'Circle2D.diameter)
    , Class.property "Radius" Circle2D.radius $(docs 'Circle2D.radius)
    , Class.member1 "Point" "Angle" (flip Circle2D.pointOn) $(docs 'Circle2D.pointOn)
    ]

type Polyline2D = Polyline2D.Polyline2D Meters FFI.Space

polyline2D :: Class
polyline2D =
  Class.new @Polyline2D $(docs ''Polyline2D.Polyline2D) $
    [ Class.constructor1 "Vertices" Polyline2D.Polyline2D $(docs 'Polyline2D.Polyline2D)
    , Class.property "Vertices" Polyline2D.vertices $(docs 'Polyline2D.vertices)
    , Class.property "Num Vertices" Polyline2D.numVertices $(docs 'Polyline2D.numVertices)
    , Class.property "Start Point" Polyline2D.startPoint $(docs 'Polyline2D.startPoint)
    , Class.property "End Point" Polyline2D.endPoint $(docs 'Polyline2D.endPoint)
    , Class.member0 "Segments" Polyline2D.segments $(docs 'Polyline2D.segments)
    , Class.member0 "Length" Polyline2D.length $(docs 'Polyline2D.length)
    ]

type UvPolyline = Polyline2D.Polyline2D Unitless UvSpace

uvPolyline :: Class
uvPolyline =
  Class.new @UvPolyline $(docs ''Polyline2D.Polyline2D) $
    [ Class.constructor1 "Vertices" Polyline2D.Polyline2D $(docs 'Polyline2D.Polyline2D)
    , Class.property "Vertices" Polyline2D.vertices $(docs 'Polyline2D.vertices)
    , Class.property "Num Vertices" Polyline2D.numVertices $(docs 'Polyline2D.numVertices)
    , Class.property "Start Point" Polyline2D.startPoint $(docs 'Polyline2D.startPoint)
    , Class.property "End Point" Polyline2D.endPoint $(docs 'Polyline2D.endPoint)
    , Class.member0 "Segments" Polyline2D.segments $(docs 'Polyline2D.segments)
    , Class.member0 "Length" Polyline2D.length $(docs 'Polyline2D.length)
    ]

type Polygon2D = Polygon2D.Polygon2D Meters FFI.Space

polygon2D :: Class
polygon2D =
  Class.new @Polygon2D $(docs ''Polygon2D.Polygon2D) $
    [ Class.constructor1 "Vertices" Polygon2D.Polygon2D $(docs 'Polygon2D.Polygon2D)
    , Class.factory2R "Inscribed" "Num Sides" "Circle" Polygon2D.inscribed $(docs 'Polygon2D.inscribed)
    , Class.factory2R "Circumscribed" "Num Sides" "Circle" Polygon2D.circumscribed $(docs 'Polygon2D.circumscribed)
    , Class.factory2R "Hexagon" "Center Point" "Height" Polygon2D.hexagon $(docs 'Polygon2D.hexagon)
    , Class.property "Vertices" Polygon2D.vertices $(docs 'Polygon2D.vertices)
    , Class.member0 "Edges" Polygon2D.edges $(docs 'Polygon2D.edges)
    , Class.member0 "Signed Area" Polygon2D.signedArea $(docs 'Polygon2D.signedArea)
    ]

type UvPolygon = Polygon2D.Polygon2D Unitless UvSpace

uvPolygon :: Class
uvPolygon =
  Class.new @UvPolygon $(docs ''Polygon2D.Polygon2D) $
    [ Class.constructor1 "Vertices" Polygon2D.Polygon2D $(docs 'Polygon2D.Polygon2D)
    , Class.factory2R "Inscribed" "Num Sides" "Circle" Polygon2D.inscribed $(docs 'Polygon2D.inscribed)
    , Class.factory2R "Circumscribed" "Num Sides" "Circle" Polygon2D.circumscribed $(docs 'Polygon2D.circumscribed)
    , Class.factory2R "Hexagon" "Center Point" "Height" Polygon2D.hexagon $(docs 'Polygon2D.hexagon)
    , Class.property "Vertices" Polygon2D.vertices $(docs 'Polygon2D.vertices)
    , Class.member0 "Edges" Polygon2D.edges $(docs 'Polygon2D.edges)
    , Class.member0 "Signed Area" Polygon2D.signedArea $(docs 'Polygon2D.signedArea)
    ]

type Curve = Curve1D.Curve1D Unitless

curve :: Class
curve =
  Class.new @Curve "A parametric curve definining a unitless value in terms of a parameter value." $
    [ Class.constant "Zero" (Curve1D.zero @Unitless) $(docs 'Curve1D.zero)
    , Class.constant "T" Curve1D.t $(docs 'Curve1D.t)
    , Class.factory1 "Constant" "Value" Curve1D.constant $(docs 'Curve1D.constant)
    , Class.factory2 "Interpolate From" "Start" "End" Curve1D.interpolateFrom $(docs 'Curve1D.interpolateFrom)
    , Class.property "Derivative" Curve1D.derivative $(docs 'Curve1D.derivative)
    , Class.member0 "Squared" Curve1D.squared $(docs 'Curve1D.squared)
    , Class.memberU0 "Sqrt" Curve1D.sqrt $(docs 'Curve1D.sqrt)
    , Class.member0 "Cubed" Curve1D.cubed $(docs 'Curve1D.cubed)
    , Class.member0 "Sin" (Curve1D.sin . (Angle.radian .*.)) $(docs 'Curve1D.sin)
    , Class.member0 "Cos" (Curve1D.cos . (Angle.radian .*.)) $(docs 'Curve1D.cos)
    , Class.member1 "Evaluate" "Parameter Value" (flip Curve1D.evaluate) $(docs 'Curve1D.evaluate)
    , Class.memberU0 "Zeros" Curve1D.zeros $(docs 'Curve1D.zeros)
    , Class.memberU0 "Is Zero" (~= Curve1D.zero) "Check if a curve is zero everywhere, within the current tolerance."
    , Class.negateSelf
    , Class.numberPlus
    , Class.numberMinus
    , Class.numberTimes
    , Class.numberDivByU (\val crv -> Curve1D.quotient (Curve1D.constant val) crv)
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
    , Class.divByU Curve1D.quotient
    , Class.nested @Curve1D.Zero "A point where a given curve is equal to zero." $
        [ Class.property "Location" (.location) "The parameter value at which the curve is zero."
        , Class.property "Order" (.order) "The order of the solution: 0 for crossing, 1 for tangent, etc."
        , Class.property "Sign" (.sign) "The sign of the solution: the sign of the curve to the right of the solution."
        ]
    ]

type AngleCurve = Curve1D.Curve1D Radians

angleCurve :: Class
angleCurve =
  Class.new @AngleCurve "A parametric curve definining an angle in terms of a parameter value." $
    [ Class.constant "Zero" (Curve1D.zero @Radians) $(docs 'Curve1D.zero)
    , Class.factory1 "Constant" "Value" Curve1D.constant $(docs 'Curve1D.constant)
    , Class.factory2 "Interpolate From" "Start" "End" Curve1D.interpolateFrom $(docs 'Curve1D.interpolateFrom)
    , Class.property "Derivative" Curve1D.derivative $(docs 'Curve1D.derivative)
    , Class.member0 "Sin" Curve1D.sin $(docs 'Curve1D.sin)
    , Class.member0 "Cos" Curve1D.cos $(docs 'Curve1D.cos)
    , Class.member1 "Evaluate" "Parameter Value" (flip Curve1D.evaluate) $(docs 'Curve1D.evaluate)
    , Class.memberR0 "Zeros" Curve1D.zeros $(docs 'Curve1D.zeros)
    , Class.memberR0 "Is Zero" (~= Curve1D.zero) "Check if a curve is zero everywhere, within the current tolerance."
    , Class.negateSelf
    , Class.numberTimes
    , Class.plusSelf
    , Class.plus @Angle Self
    , Class.minusSelf
    , Class.minus @Angle Self
    , Class.timesNumber
    , Class.times @Curve Self
    , Class.divByNumber
    , Class.divByR Curve1D.quotient
    , Class.divBy @Angle Self
    , Class.divByU Curve1D.quotient
    ]

type LengthCurve = Curve1D.Curve1D Meters

lengthCurve :: Class
lengthCurve =
  Class.new @LengthCurve "A parametric curve definining a length in terms of a parameter value." $
    [ Class.constant "Zero" (Curve1D.zero @Meters) $(docs 'Curve1D.zero)
    , Class.factory1 "Constant" "Value" Curve1D.constant $(docs 'Curve1D.constant)
    , Class.factory2 "Interpolate From" "Start" "End" Curve1D.interpolateFrom $(docs 'Curve1D.interpolateFrom)
    , Class.property "Derivative" Curve1D.derivative $(docs 'Curve1D.derivative)
    , Class.member0 "Squared" Curve1D.squared $(docs 'Curve1D.squared)
    , Class.member1 "Evaluate" "Parameter Value" (flip Curve1D.evaluate) $(docs 'Curve1D.evaluate)
    , Class.memberM0 "Zeros" Curve1D.zeros $(docs 'Curve1D.zeros)
    , Class.memberM0 "Is Zero" (~= Curve1D.zero) "Check if a curve is zero everywhere, within the current tolerance."
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
    , Class.divByM Curve1D.quotient
    , Class.divBy @Length Self
    , Class.divByU Curve1D.quotient
    ]

type AreaCurve = Curve1D.Curve1D SquareMeters

areaCurve :: Class
areaCurve =
  Class.new @AreaCurve "A parametric curve definining an area in terms of a parameter value." $
    [ Class.constant "Zero" (Curve1D.zero @SquareMeters) $(docs 'Curve1D.zero)
    , Class.factory1 "Constant" "Value" Curve1D.constant $(docs 'Curve1D.constant)
    , Class.factory2 "Interpolate From" "Start" "End" Curve1D.interpolateFrom $(docs 'Curve1D.interpolateFrom)
    , Class.property "Derivative" Curve1D.derivative $(docs 'Curve1D.derivative)
    , Class.memberM0 "Sqrt" Curve1D.sqrt $(docs 'Curve1D.sqrt)
    , Class.member1 "Evaluate" "Parameter Value" (flip Curve1D.evaluate) $(docs 'Curve1D.evaluate)
    , Class.memberS0 "Zeros" Curve1D.zeros $(docs 'Curve1D.zeros)
    , Class.memberS0 "Is Zero" (~= Curve1D.zero) "Check if a curve is zero everywhere, within the current tolerance."
    , Class.negateSelf
    , Class.numberTimes
    , Class.plusSelf
    , Class.plus @Area Self
    , Class.minusSelf
    , Class.minus @Area Self
    , Class.timesNumber
    , Class.times @Curve Self
    , Class.divByNumber
    , Class.divByS Curve1D.quotient
    , Class.divBy @Length Self
    , Class.divBy @Area Self
    , Class.divByU Curve1D.quotient
    , Class.divByM Curve1D.quotient
    ]

type Svg = Svg.Svg FFI.Space

svg :: Class
svg =
  Class.new @Svg $(docs ''Svg.Svg) $
    [ Class.member1 "To Text" "View Box" Svg.toText $(docs 'Svg.toText)
    , Class.member2 "Write" "Path" "View Box" Svg.write $(docs 'Svg.write)
    , Class.factory1 "Group" "Children" Svg.group $(docs 'Svg.group)
    , Class.factory2 "Group With" "Attributes" "Children" Svg.groupWith $(docs 'Svg.groupWith)
    , Class.factory1 "Line" "Line" Svg.line $(docs 'Svg.line)
    , Class.factory2 "Line With" "Attributes" "Line" Svg.lineWith $(docs 'Svg.lineWith)
    , Class.factory1 "Polyline" "Polyline" Svg.polyline $(docs 'Svg.polyline)
    , Class.factory2 "Polyline With" "Attributes" "Polyline" Svg.polylineWith $(docs 'Svg.polylineWith)
    , Class.factory1 "Polygon" "Polygon" Svg.polygon $(docs 'Svg.polygon)
    , Class.factory2 "Polygon With" "Attributes" "Polygon" Svg.polygonWith $(docs 'Svg.polygonWith)
    , Class.factory1 "Triangle" "Triangle" Svg.triangle $(docs 'Svg.triangle)
    , Class.factory2 "Triangle With" "Attributes" "Triangle" Svg.triangleWith $(docs 'Svg.triangleWith)
    , Class.factory2 "Circle" "Center Point" "Diameter" Svg.circle $(docs 'Svg.circle)
    , Class.factory3 "Circle With" "Attributes" "Center Point" "Diameter" Svg.circleWith $(docs 'Svg.circleWith)
    , Class.factory2 "Curve" "Resolution" "Curve" Svg.curve $(docs 'Svg.curve)
    , Class.factory3 "Curve With" "Attributes" "Resolution" "Curve" Svg.curveWith $(docs 'Svg.curveWith)
    , Class.constant "Black Stroke" (Svg.blackStroke @FFI.Space) $(docs 'Svg.blackStroke)
    , Class.static1 "Stroke Color" "Color" (Svg.strokeColor @FFI.Space) $(docs 'Svg.strokeColor)
    , Class.constant "No Fill" (Svg.noFill @FFI.Space) $(docs 'Svg.noFill)
    , Class.static1 "Fill Color" "Color" (Svg.fillColor @FFI.Space) $(docs 'Svg.fillColor)
    , Class.nested @(Svg.Attribute FFI.Space) $(docs ''Svg.Attribute) []
    ]

type Axis2D = Axis2D.Axis2D Meters FFI.Space

axis2D :: Class
axis2D =
  Class.new @Axis2D $(docs ''Axis2D.Axis2D) $
    [ Class.constructor2 "Origin Point" "Direction" Axis2D.Axis2D $(docs 'Axis2D.Axis2D)
    , Class.property "Origin Point" (.originPoint) $(docs 'Axis2D.originPoint)
    , Class.property "Direction" (.direction) $(docs 'Axis2D.direction)
    , Class.constant "X" (Axis2D.x :: Axis2D) $(docs 'Axis2D.x)
    , Class.constant "Y" (Axis2D.y :: Axis2D) $(docs 'Axis2D.y)
    , Class.member1 "Place On" "Plane" (Axis2D.placeOn :: Plane3D -> Axis2D -> Axis3D) $(docs 'Axis2D.placeOn)
    ]
      <> orthonormalTransformations2D Axis2D.transformBy

type UvAxis = Axis2D.Axis2D Unitless UvSpace

uvAxis :: Class
uvAxis =
  Class.new @UvAxis $(docs ''Axis2D.Axis2D) $
    [ Class.constructor2 "Origin Point" "Direction" Axis2D.Axis2D $(docs 'Axis2D.Axis2D)
    , Class.property "Origin Point" (.originPoint) $(docs 'Axis2D.originPoint)
    , Class.property "Direction" (.direction) $(docs 'Axis2D.direction)
    , Class.constant "U" (Axis2D.x :: UvAxis) "The U axis."
    , Class.constant "V" (Axis2D.y :: UvAxis) "The V axis."
    ]

world3D :: Class
world3D =
  Class.static "World3D" "A collection of global datums." $
    [ Class.constant "Origin Point" (World3D.originPoint :: Point3D) $(docs 'World3D.originPoint)
    , Class.constant "Forward Direction" (World3D.forwardDirection :: Direction3D) $(docs 'World3D.forwardDirection)
    , Class.constant "Backward Direction" (World3D.backwardDirection :: Direction3D) $(docs 'World3D.backwardDirection)
    , Class.constant "Leftward Direction" (World3D.leftwardDirection :: Direction3D) $(docs 'World3D.leftwardDirection)
    , Class.constant "Rightward Direction" (World3D.rightwardDirection :: Direction3D) $(docs 'World3D.rightwardDirection)
    , Class.constant "Upward Direction" (World3D.upwardDirection :: Direction3D) $(docs 'World3D.upwardDirection)
    , Class.constant "Downward Direction" (World3D.downwardDirection :: Direction3D) $(docs 'World3D.downwardDirection)
    , Class.constant "Forward Orientation" (World3D.forwardOrientation :: Orientation3D) $(docs 'World3D.forwardOrientation)
    , Class.constant "Backward Orientation" (World3D.backwardOrientation :: Orientation3D) $(docs 'World3D.backwardOrientation)
    , Class.constant "Leftward Orientation" (World3D.leftwardOrientation :: Orientation3D) $(docs 'World3D.leftwardOrientation)
    , Class.constant "Rightward Orientation" (World3D.rightwardOrientation :: Orientation3D) $(docs 'World3D.rightwardOrientation)
    , Class.constant "Upward Orientation" (World3D.upwardOrientation :: Orientation3D) $(docs 'World3D.upwardOrientation)
    , Class.constant "Downward Orientation" (World3D.downwardOrientation :: Orientation3D) $(docs 'World3D.downwardOrientation)
    , Class.constant "Frame" (World3D.frame :: Frame3D) $(docs 'World3D.frame)
    , Class.constant "Forward Axis" (World3D.forwardAxis :: Axis3D) $(docs 'World3D.forwardAxis)
    , Class.constant "Backward Axis" (World3D.backwardAxis :: Axis3D) $(docs 'World3D.backwardAxis)
    , Class.constant "Leftward Axis" (World3D.leftwardAxis :: Axis3D) $(docs 'World3D.leftwardAxis)
    , Class.constant "Rightward Axis" (World3D.rightwardAxis :: Axis3D) $(docs 'World3D.rightwardAxis)
    , Class.constant "Upward Axis" (World3D.upwardAxis :: Axis3D) $(docs 'World3D.upwardAxis)
    , Class.constant "Downward Axis" (World3D.downwardAxis :: Axis3D) $(docs 'World3D.downwardAxis)
    , Class.constant "Front Plane" (World3D.frontPlane :: Plane3D) $(docs 'World3D.frontPlane)
    , Class.constant "Back Plane" (World3D.backPlane :: Plane3D) $(docs 'World3D.backPlane)
    , Class.constant "Left Plane" (World3D.leftPlane :: Plane3D) $(docs 'World3D.leftPlane)
    , Class.constant "Right Plane" (World3D.rightPlane :: Plane3D) $(docs 'World3D.rightPlane)
    , Class.constant "Top Plane" (World3D.topPlane :: Plane3D) $(docs 'World3D.topPlane)
    , Class.constant "Bottom Plane" (World3D.bottomPlane :: Plane3D) $(docs 'World3D.bottomPlane)
    ]

convention3D :: Class
convention3D =
  Class.new @Convention3D $(docs ''Convention3D) $
    [ Class.constant "Y Up" Convention3D.yUp $(docs 'Convention3D.yUp)
    , Class.constant "Z Up" Convention3D.zUp $(docs 'Convention3D.zUp)
    ]

type Vector3D = Vector3D.Vector3D Unitless FFI.Space

vector3D :: Class
vector3D =
  Class.new @Vector3D "A unitless vector in 3D." $
    [ Class.constant "Zero" (Vector3D.zero :: Vector3D) $(docs 'Vector3D.zero)
    , Class.factory1 "Unit" "Direction" Vector3D.unit $(docs 'Vector3D.unit)
    , Class.factory2 "XYZ" "Convention" "Components" Vector3D.xyz $(docs 'Vector3D.xyz)
    , Class.factory3 "Z Up" "X Component" "Y Component" "Z Component" Vector3D.zUp $(docs 'Vector3D.zUp)
    , Class.factory3 "Y Up" "X Component" "Y Component" "Z Component" Vector3D.yUp $(docs 'Vector3D.yUp)
    , Class.member1 "Components" "Convention" Vector3D.components $(docs 'Vector3D.components)
    , Class.member0 "Z Up Components" Vector3D.zUpComponents $(docs 'Vector3D.zUpComponents)
    , Class.member0 "Y Up Components" Vector3D.yUpComponents $(docs 'Vector3D.yUpComponents)
    , Class.memberU0 "Direction" Vector3D.direction $(docs 'Vector3D.direction)
    , Class.memberU0 "Is Zero" (~= Vector3D.zero) "Check if a vector is zero, within the current tolerance."
    , Class.member2 "Rotate In" "Direction" "Angle" Vector3D.rotateIn $(docs 'Vector3D.rotateIn)
    , Class.member2 "Rotate Around" "Axis" "Angle" (Vector3D.rotateAround :: Axis3D -> Angle -> Vector3D -> Vector3D) $(docs 'Vector3D.rotateAround)
    , Class.member1 "Mirror In" "Direction" Vector3D.mirrorIn $(docs 'Vector3D.mirrorIn)
    , Class.member1 "Mirror Across" "Plane" (Vector3D.mirrorAcross :: Plane3D -> Vector3D -> Vector3D) $(docs 'Vector3D.mirrorAcross)
    , Class.member2 "Scale In" "Direction" "Scale" Vector3D.scaleIn $(docs 'Vector3D.scaleIn)
    , Class.member2 "Scale Along" "Axis" "Scale" (Vector3D.scaleAlong :: Axis3D -> Number -> Vector3D -> Vector3D) $(docs 'Vector3D.scaleAlong)
    , Class.member1 "Place In" "Frame" (Vector3D.placeIn :: Frame3D -> Vector3D -> Vector3D) $(docs 'Vector3D.placeIn)
    , Class.member1 "Relative To" "Frame" (Vector3D.relativeTo :: Frame3D -> Vector3D -> Vector3D) $(docs 'Vector3D.relativeTo)
    , Class.negateSelf
    , Class.numberTimes
    , Class.plusSelf
    , Class.minusSelf
    , Class.timesNumber
    , Class.times @Length Self
    , Class.times @Area Self
    , Class.divByNumber
    , Class.dotSelf
    , Class.dotProduct @Displacement3D Self
    , Class.dotProduct @AreaVector3D Self
    , Class.crossSelf
    , Class.crossProduct @Displacement3D Self
    , Class.crossProduct @AreaVector3D Self
    ]

type Displacement3D = Vector3D.Vector3D Meters FFI.Space

displacement3D :: Class
displacement3D =
  Class.new @Displacement3D "A displacement vector in 3D." $
    [ Class.constant "Zero" (Vector3D.zero :: Displacement3D) $(docs 'Vector3D.zero)
    , Class.factory2 "XYZ" "Convention" "Components" Vector3D.xyz $(docs 'Vector3D.xyz)
    , Class.factory3 "Z Up" "X Component" "Y Component" "Z Component" Vector3D.zUp $(docs 'Vector3D.zUp)
    , Class.factory3 "Y Up" "X Component" "Y Component" "Z Component" Vector3D.yUp $(docs 'Vector3D.yUp)
    , Class.member1 "Components" "Convention" Vector3D.components $(docs 'Vector3D.components)
    , Class.member0 "Z Up Components" Vector3D.zUpComponents $(docs 'Vector3D.zUpComponents)
    , Class.member0 "Y Up Components" Vector3D.yUpComponents $(docs 'Vector3D.yUpComponents)
    , Class.memberM0 "Direction" Vector3D.direction $(docs 'Vector3D.direction)
    , Class.memberM0 "Is Zero" (~= Vector3D.zero) "Check if a displacement is zero, within the current tolerance."
    , Class.member2 "Rotate In" "Direction" "Angle" Vector3D.rotateIn $(docs 'Vector3D.rotateIn)
    , Class.member2 "Rotate Around" "Axis" "Angle" (Vector3D.rotateAround :: Axis3D -> Angle -> Displacement3D -> Displacement3D) $(docs 'Vector3D.rotateAround)
    , Class.member1 "Mirror In" "Direction" Vector3D.mirrorIn $(docs 'Vector3D.mirrorIn)
    , Class.member1 "Mirror Across" "Plane" (Vector3D.mirrorAcross :: Plane3D -> Displacement3D -> Displacement3D) $(docs 'Vector3D.mirrorAcross)
    , Class.member2 "Scale In" "Direction" "Scale" Vector3D.scaleIn $(docs 'Vector3D.scaleIn)
    , Class.member2 "Scale Along" "Axis" "Scale" (Vector3D.scaleAlong :: Axis3D -> Number -> Displacement3D -> Displacement3D) $(docs 'Vector3D.scaleAlong)
    , Class.member1 "Place In" "Frame" (Vector3D.placeIn :: Frame3D -> Displacement3D -> Displacement3D) $(docs 'Vector3D.placeIn)
    , Class.member1 "Relative To" "Frame" (Vector3D.relativeTo :: Frame3D -> Displacement3D -> Displacement3D) $(docs 'Vector3D.relativeTo)
    , Class.negateSelf
    , Class.numberTimes
    , Class.plusSelf
    , Class.minusSelf
    , Class.timesNumber
    , Class.times @Length Self
    , Class.divByNumber
    , Class.divBy @Length Self
    , Class.dotSelf
    , Class.dotProduct @Vector3D Self
    , Class.crossSelf
    , Class.crossProduct @Vector3D Self
    ]

type AreaVector3D = Vector3D.Vector3D SquareMeters FFI.Space

areaVector3D :: Class
areaVector3D =
  Class.new @AreaVector3D "A vector in 3D with units of area." $
    [ Class.constant "Zero" (Vector3D.zero :: AreaVector3D) $(docs 'Vector3D.zero)
    , Class.factory2 "XYZ" "Convention" "Components" Vector3D.xyz $(docs 'Vector3D.xyz)
    , Class.factory3 "Z Up" "X Component" "Y Component" "Z Component" Vector3D.zUp $(docs 'Vector3D.zUp)
    , Class.factory3 "Y Up" "X Component" "Y Component" "Z Component" Vector3D.yUp $(docs 'Vector3D.yUp)
    , Class.member1 "Components" "Convention" Vector3D.components $(docs 'Vector3D.components)
    , Class.member0 "Z Up Components" Vector3D.zUpComponents $(docs 'Vector3D.zUpComponents)
    , Class.member0 "Y Up Components" Vector3D.yUpComponents $(docs 'Vector3D.yUpComponents)
    , Class.memberS0 "Direction" Vector3D.direction $(docs 'Vector3D.direction)
    , Class.memberS0 "Is Zero" (~= Vector3D.zero) "Check if an area vector is zero, within the current tolerance."
    , Class.member2 "Rotate In" "Direction" "Angle" Vector3D.rotateIn $(docs 'Vector3D.rotateIn)
    , Class.member2 "Rotate Around" "Axis" "Angle" (Vector3D.rotateAround :: Axis3D -> Angle -> AreaVector3D -> AreaVector3D) $(docs 'Vector3D.rotateAround)
    , Class.member1 "Mirror In" "Direction" Vector3D.mirrorIn $(docs 'Vector3D.mirrorIn)
    , Class.member1 "Mirror Across" "Plane" (Vector3D.mirrorAcross :: Plane3D -> AreaVector3D -> AreaVector3D) $(docs 'Vector3D.mirrorAcross)
    , Class.member2 "Scale In" "Direction" "Scale" Vector3D.scaleIn $(docs 'Vector3D.scaleIn)
    , Class.member2 "Scale Along" "Axis" "Scale" (Vector3D.scaleAlong :: Axis3D -> Number -> AreaVector3D -> AreaVector3D) $(docs 'Vector3D.scaleAlong)
    , Class.member1 "Place In" "Frame" (Vector3D.placeIn :: Frame3D -> AreaVector3D -> AreaVector3D) $(docs 'Vector3D.placeIn)
    , Class.member1 "Relative To" "Frame" (Vector3D.relativeTo :: Frame3D -> AreaVector3D -> AreaVector3D) $(docs 'Vector3D.relativeTo)
    , Class.negateSelf
    , Class.numberTimes
    , Class.plusSelf
    , Class.minusSelf
    , Class.timesNumber
    , Class.divByNumber
    , Class.divBy @Length Self
    , Class.divBy @Area Self
    , Class.dotProduct @Vector3D Self
    , Class.crossProduct @Vector3D Self
    ]

type Direction3D = Direction3D.Direction3D FFI.Space

direction3D :: Class
direction3D =
  Class.new @Direction3D $(docs ''Direction3D.Direction3D) $
    [ Class.upcast Vector3D.unit
    , Class.member0 "Perpendicular Direction" Direction3D.perpendicularDirection $(docs 'Direction3D.perpendicularDirection)
    , Class.member1 "Angle To" "Other" Direction3D.angleFrom $(docs 'Direction3D.angleFrom)
    , Class.member2 "Rotate In" "Direction" "Angle" Direction3D.rotateIn $(docs 'Direction3D.rotateIn)
    , Class.member2 "Rotate Around" "Axis" "Angle" (Direction3D.rotateAround :: Axis3D -> Angle -> Direction3D -> Direction3D) $(docs 'Direction3D.rotateAround)
    , Class.member1 "Mirror In" "Direction" Direction3D.mirrorIn $(docs 'Direction3D.mirrorIn)
    , Class.member1 "Mirror Across" "Plane" (Direction3D.mirrorAcross :: Plane3D -> Direction3D -> Direction3D) $(docs 'Direction3D.mirrorAcross)
    , Class.member1 "Place In" "Frame" (Direction3D.placeIn :: Frame3D -> Direction3D -> Direction3D) $(docs 'Direction3D.placeIn)
    , Class.member1 "Relative To" "Frame" (Direction3D.relativeTo :: Frame3D -> Direction3D -> Direction3D) $(docs 'Direction3D.relativeTo)
    , Class.negateSelf
    ]

type Point3D = Point3D.Point3D FFI.Space

point3D :: Class
point3D =
  Class.new @Point3D "A point in 3D." $
    [ Class.factory2 "Along" "Axis" "Distance" Point3D.along $(docs 'Point3D.along)
    , Class.factory2 "XYZ" "Convention" "Coordinates" Point3D.xyz $(docs 'Point3D.xyz)
    , Class.factory3 "Z Up" "X Coordinate" "Y Coordinate" "Z Coordinate" Point3D.zUp $(docs 'Point3D.zUp)
    , Class.factory3 "Y Up" "X Coordinate" "Y Coordinate" "Z Coordinate" Point3D.yUp $(docs 'Point3D.yUp)
    , Class.member1 "Coordinates" "Convention" Point3D.coordinates $(docs 'Point3D.coordinates)
    , Class.member0 "Z Up Coordinates" Point3D.zUpCoordinates $(docs 'Point3D.zUpCoordinates)
    , Class.member0 "Y Up Coordinates" Point3D.yUpCoordinates $(docs 'Point3D.yUpCoordinates)
    , Class.member1 "Distance To" "Other" Point3D.distanceFrom $(docs 'Point3D.distanceFrom)
    , Class.member1 "Midpoint" "Other" Point3D.midpoint $(docs 'Point3D.midpoint)
    , Class.member1 "Project Onto" "Plane" Point3D.projectOnto $(docs 'Point3D.projectOnto)
    , Class.member1 "Project Into" "Plane" Point3D.projectInto $(docs 'Point3D.projectInto)
    , Class.minusSelf
    , Class.minus @Displacement3D Self
    , Class.plus @Displacement3D Self
    , Class.member1 "Place In" "Frame" (Point3D.placeIn :: Frame3D -> Point3D -> Point3D) $(docs 'Point3D.placeIn)
    , Class.member1 "Relative To" "Frame" (Point3D.relativeTo :: Frame3D -> Point3D -> Point3D) $(docs 'Point3D.relativeTo)
    ]
      <> affineTransformations3D Point3D.transformBy

type Bounds3D = Bounds3D.Bounds3D FFI.Space

bounds3D :: Class
bounds3D =
  Class.new @Bounds3D $(docs ''Bounds3D.Bounds3D) $
    [ Class.factory1 "Constant" "Point" Bounds3D.constant $(docs 'Bounds3D.constant)
    , Class.factory2 "From Corners" "First Point" "Second Point" Bounds3D.hull2 $(docs 'Bounds3D.hull2)
    , Class.factory1 @(NonEmpty Point3D) "Hull" "Points" Bounds3D.hullN $(docs 'Bounds3D.hullN)
    , Class.factory1 "Aggregate" "Bounds" Bounds3D.aggregateN $(docs 'Bounds3D.aggregateN)
    , Class.member1 "Coordinates" "Convention" Bounds3D.coordinates $(docs 'Bounds3D.coordinates)
    , Class.plus @Displacement3D Self
    , Class.minus @Displacement3D Self
    ]
      <> affineTransformations3D Bounds3D.transformBy

type Axis3D = Axis3D.Axis3D FFI.Space

axis3D :: Class
axis3D =
  Class.new @Axis3D $(docs ''Axis3D.Axis3D) $
    [ Class.constructor2 "Origin Point" "Direction" Axis3D.Axis3D $(docs 'Axis3D.Axis3D)
    , Class.property "Origin Point" (.originPoint) $(docs 'Axis3D.originPoint)
    , Class.property "Direction" (.direction) $(docs 'Axis3D.direction)
    , Class.member0 "Normal Plane" (Axis3D.normalPlane :: Axis3D -> Plane3D) $(docs 'Axis3D.normalPlane)
    , Class.member1 "Move To" "Point" Axis3D.moveTo $(docs 'Axis3D.moveTo)
    , Class.member0 "Reverse" Axis3D.reverse $(docs 'Axis3D.reverse)
    , Class.member1 "Place In" "Frame" (Axis3D.placeIn :: Frame3D -> Axis3D -> Axis3D) $(docs 'Axis3D.placeIn)
    , Class.member1 "Relative To" "Frame" (Axis3D.relativeTo :: Frame3D -> Axis3D -> Axis3D) $(docs 'Axis3D.relativeTo)
    ]
      <> orthonormalTransformations3D Axis3D.transformBy

type PlaneOrientation3D = PlaneOrientation3D.PlaneOrientation3D FFI.Space

planeOrientation3D :: Class
planeOrientation3D =
  Class.new @PlaneOrientation3D $(docs ''PlaneOrientation3D.PlaneOrientation3D) $
    [ Class.factory1 "From Normal Direction" "Direction" PlaneOrientation3D.fromNormalDirection $(docs 'PlaneOrientation3D.fromNormalDirection)
    , Class.factory1 "From X Direction" "Direction" PlaneOrientation3D.fromXDirection $(docs 'PlaneOrientation3D.fromXDirection)
    , Class.factory1 "From Y Direction" "Direction" PlaneOrientation3D.fromYDirection $(docs 'PlaneOrientation3D.fromYDirection)
    , Class.property "X Direction" (.xDirection) $(docs 'PlaneOrientation3D.xDirection)
    , Class.property "Y Direction" (.yDirection) $(docs 'PlaneOrientation3D.yDirection)
    , Class.property "Normal Direction" (.normalDirection) $(docs 'PlaneOrientation3D.normalDirection)
    , Class.member1 "Place In" "Frame" (PlaneOrientation3D.placeIn :: Frame3D -> PlaneOrientation3D -> PlaneOrientation3D) $(docs 'PlaneOrientation3D.placeIn)
    , Class.member1 "Relative To" "Frame" (PlaneOrientation3D.relativeTo :: Frame3D -> PlaneOrientation3D -> PlaneOrientation3D) $(docs 'PlaneOrientation3D.relativeTo)
    ]

type Plane3D = Plane3D.Plane3D FFI.Space FFI.Space

plane3D :: Class
plane3D =
  Class.new @Plane3D $(docs ''Plane3D.Plane3D) $
    [ Class.factory2 "From Point And Normal" "Origin Point" "Normal Direction" Plane3D.fromPointAndNormal $(docs 'Plane3D.fromPointAndNormal)
    , Class.factory1 "From X Axis" "Axis" Plane3D.fromXAxis $(docs 'Plane3D.fromXAxis)
    , Class.factory1 "From Y Axis" "Axis" Plane3D.fromYAxis $(docs 'Plane3D.fromYAxis)
    , Class.property "Origin Point" (.originPoint) $(docs 'Plane3D.originPoint)
    , Class.property "X Direction" (.xDirection) $(docs 'Plane3D.xDirection)
    , Class.property "Y Direction" (.yDirection) $(docs 'Plane3D.yDirection)
    , Class.property "Normal Direction" (.normalDirection) $(docs 'Plane3D.normalDirection)
    , Class.property "X Axis" (.xAxis) $(docs 'Plane3D.xAxis)
    , Class.property "Y Axis" (.yAxis) $(docs 'Plane3D.yAxis)
    , Class.property "Normal Axis" (.normalAxis) $(docs 'Plane3D.normalAxis)
    , Class.member1 "Move To" "Point" Plane3D.moveTo $(docs 'Plane3D.moveTo)
    , Class.member0 "Flip" Plane3D.flip $(docs 'Plane3D.flip)
    , Class.member1 "Offset By" "Distance" Plane3D.offsetBy $(docs 'Plane3D.offsetBy)
    , Class.member1 "Place In" "Frame" (Plane3D.placeIn :: Frame3D -> Plane3D -> Plane3D) $(docs 'Plane3D.placeIn)
    , Class.member1 "Relative To" "Frame" (Plane3D.relativeTo :: Frame3D -> Plane3D -> Plane3D) $(docs 'Plane3D.relativeTo)
    ]
      <> rigidTransformations3D Plane3D.transformBy

type Orientation3D = Orientation3D.Orientation3D FFI.Space

orientation3D :: Class
orientation3D =
  Class.new @Orientation3D $(docs ''Orientation3D.Orientation3D) $
    [ Class.property "Forward Direction" (.forwardDirection) $(docs 'Orientation3D.forwardDirection)
    , Class.property "Backward Direction" (.backwardDirection) $(docs 'Orientation3D.backwardDirection)
    , Class.property "Leftward Direction" (.leftwardDirection) $(docs 'Orientation3D.leftwardDirection)
    , Class.property "Rightward Direction" (.rightwardDirection) $(docs 'Orientation3D.rightwardDirection)
    , Class.property "Upward Direction" (.upwardDirection) $(docs 'Orientation3D.upwardDirection)
    , Class.property "Downward Direction" (.downwardDirection) $(docs 'Orientation3D.downwardDirection)
    , Class.property "Front Plane Orientation" (.frontPlaneOrientation) $(docs 'Orientation3D.frontPlaneOrientation)
    , Class.property "Back Plane Orientation" (.backPlaneOrientation) $(docs 'Orientation3D.backPlaneOrientation)
    , Class.property "Right Plane Orientation" (.rightPlaneOrientation) $(docs 'Orientation3D.rightPlaneOrientation)
    , Class.property "Left Plane Orientation" (.leftPlaneOrientation) $(docs 'Orientation3D.leftPlaneOrientation)
    , Class.property "Bottom Plane Orientation" (.bottomPlaneOrientation) $(docs 'Orientation3D.bottomPlaneOrientation)
    , Class.property "Top Plane Orientation" (.topPlaneOrientation) $(docs 'Orientation3D.topPlaneOrientation)
    , Class.property "Backward Orientation" (.backwardOrientation) $(docs 'Orientation3D.backwardOrientation)
    , Class.property "Rightward Orientation" (.rightwardOrientation) $(docs 'Orientation3D.rightwardOrientation)
    , Class.property "Leftward Orientation" (.leftwardOrientation) $(docs 'Orientation3D.leftwardOrientation)
    , Class.property "Upward Orientation" (.upwardOrientation) $(docs 'Orientation3D.upwardOrientation)
    , Class.property "Downward Orientation" (.downwardOrientation) $(docs 'Orientation3D.downwardOrientation)
    , Class.member1 "Place In" "Frame" (Orientation3D.placeIn :: Frame3D -> Orientation3D -> Orientation3D) $(docs 'Orientation3D.placeIn)
    , Class.member1 "Relative To" "Frame" (Orientation3D.relativeTo :: Frame3D -> Orientation3D -> Orientation3D) $(docs 'Orientation3D.relativeTo)
    ]

type Frame3D = Frame3D.Frame3D FFI.Space FFI.Space

frame3D :: Class
frame3D =
  Class.new @Frame3D $(docs ''Frame3D.Frame3D) $
    [ Class.factory1 "From Front Plane" "Plane" (Frame3D.fromFrontPlane :: Plane3D -> Frame3D) $(docs 'Frame3D.fromFrontPlane)
    , Class.factory1 "From Back Plane" "Plane" (Frame3D.fromBackPlane :: Plane3D -> Frame3D) $(docs 'Frame3D.fromBackPlane)
    , Class.factory1 "From Right Plane" "Plane" (Frame3D.fromRightPlane :: Plane3D -> Frame3D) $(docs 'Frame3D.fromRightPlane)
    , Class.factory1 "From Left Plane" "Plane" (Frame3D.fromLeftPlane :: Plane3D -> Frame3D) $(docs 'Frame3D.fromLeftPlane)
    , Class.factory1 "From Top Plane" "Plane" (Frame3D.fromTopPlane :: Plane3D -> Frame3D) $(docs 'Frame3D.fromTopPlane)
    , Class.factory1 "From Bottom Plane" "Plane" (Frame3D.fromBottomPlane :: Plane3D -> Frame3D) $(docs 'Frame3D.fromBottomPlane)
    , Class.factory2 "Align" "Frame" "Reference Frame" (Frame3D.align :: Frame3D -> Frame3D -> Frame3D) $(docs 'Frame3D.align)
    , Class.factory2 "Mate" "Frame" "Reference Frame" (Frame3D.mate :: Frame3D -> Frame3D -> Frame3D) $(docs 'Frame3D.mate)
    , Class.property "Origin Point" (.originPoint) $(docs 'Frame3D.originPoint)
    , Class.property "Forward Direction" (.forwardDirection) $(docs 'Frame3D.forwardDirection)
    , Class.property "Backward Direction" (.backwardDirection) $(docs 'Frame3D.backwardDirection)
    , Class.property "Rightward Direction" (.rightwardDirection) $(docs 'Frame3D.rightwardDirection)
    , Class.property "Leftward Direction" (.leftwardDirection) $(docs 'Frame3D.leftwardDirection)
    , Class.property "Upward Direction" (.upwardDirection) $(docs 'Frame3D.upwardDirection)
    , Class.property "Downward Direction" (.downwardDirection) $(docs 'Frame3D.downwardDirection)
    , Class.property "Forward Axis" (.forwardAxis) $(docs 'Frame3D.forwardAxis)
    , Class.property "Backward Axis" (.backwardAxis) $(docs 'Frame3D.backwardAxis)
    , Class.property "Rightward Axis" (.rightwardAxis) $(docs 'Frame3D.rightwardAxis)
    , Class.property "Leftward Axis" (.leftwardAxis) $(docs 'Frame3D.leftwardAxis)
    , Class.property "Upward Axis" (.upwardAxis) $(docs 'Frame3D.upwardAxis)
    , Class.property "Downward Axis" (.downwardAxis) $(docs 'Frame3D.downwardAxis)
    , Class.property "Front Plane" (Frame3D.frontPlane :: Frame3D -> Plane3D) $(docs 'Frame3D.frontPlane)
    , Class.property "Back Plane" (Frame3D.backPlane :: Frame3D -> Plane3D) $(docs 'Frame3D.backPlane)
    , Class.property "Right Plane" (Frame3D.rightPlane :: Frame3D -> Plane3D) $(docs 'Frame3D.rightPlane)
    , Class.property "Left Plane" (Frame3D.leftPlane :: Frame3D -> Plane3D) $(docs 'Frame3D.leftPlane)
    , Class.property "Top Plane" (Frame3D.topPlane :: Frame3D -> Plane3D) $(docs 'Frame3D.topPlane)
    , Class.property "Bottom Plane" (Frame3D.bottomPlane :: Frame3D -> Plane3D) $(docs 'Frame3D.bottomPlane)
    , Class.property "Backward Frame" (Frame3D.backward :: Frame3D -> Frame3D) $(docs 'Frame3D.backward)
    , Class.property "Leftward Frame" (Frame3D.leftward :: Frame3D -> Frame3D) $(docs 'Frame3D.leftward)
    , Class.property "Rightward Frame" (Frame3D.rightward :: Frame3D -> Frame3D) $(docs 'Frame3D.rightward)
    , Class.property "Upward Frame" (Frame3D.upward :: Frame3D -> Frame3D) $(docs 'Frame3D.upward)
    , Class.property "Downward Frame" (Frame3D.downward :: Frame3D -> Frame3D) $(docs 'Frame3D.downward)
    , Class.member1 "Place In" "Other Frame" (Frame3D.placeIn :: Frame3D -> Frame3D -> Frame3D) $(docs 'Frame3D.placeIn)
    , Class.member1 "Relative To" "Other Frame" (Frame3D.relativeTo :: Frame3D -> Frame3D -> Frame3D) $(docs 'Frame3D.relativeTo)
    , Class.member0 "Inverse" Frame3D.inverse $(docs 'Frame3D.inverse)
    , Class.member1 "Move To" "Point" (Frame3D.moveTo :: Point3D -> Frame3D -> Frame3D) $(docs 'Frame3D.moveTo)
    , Class.member1 "Offset Forward By" "Distance" (Frame3D.offsetForwardBy :: Length -> Frame3D -> Frame3D) $(docs 'Frame3D.offsetForwardBy)
    , Class.member1 "Offset Backward By" "Distance" (Frame3D.offsetBackwardBy :: Length -> Frame3D -> Frame3D) $(docs 'Frame3D.offsetBackwardBy)
    , Class.member1 "Offset Rightward By" "Distance" (Frame3D.offsetRightwardBy :: Length -> Frame3D -> Frame3D) $(docs 'Frame3D.offsetRightwardBy)
    , Class.member1 "Offset Leftward By" "Distance" (Frame3D.offsetLeftwardBy :: Length -> Frame3D -> Frame3D) $(docs 'Frame3D.offsetLeftwardBy)
    , Class.member1 "Offset Upward By" "Distance" (Frame3D.offsetUpwardBy :: Length -> Frame3D -> Frame3D) $(docs 'Frame3D.offsetUpwardBy)
    , Class.member1 "Offset Downward By" "Distance" (Frame3D.offsetDownwardBy :: Length -> Frame3D -> Frame3D) $(docs 'Frame3D.offsetDownwardBy)
    , Class.member1 "Turn Right By" "Angle" (Frame3D.turnRightBy :: Angle -> Frame3D -> Frame3D) $(docs 'Frame3D.turnRightBy)
    , Class.member1 "Turn Left By" "Angle" (Frame3D.turnLeftBy :: Angle -> Frame3D -> Frame3D) $(docs 'Frame3D.turnLeftBy)
    , Class.member1 "Roll Right By" "Angle" (Frame3D.rollRightBy :: Angle -> Frame3D -> Frame3D) $(docs 'Frame3D.rollRightBy)
    , Class.member1 "Roll Left By" "Angle" (Frame3D.rollLeftBy :: Angle -> Frame3D -> Frame3D) $(docs 'Frame3D.rollLeftBy)
    , Class.member1 "Tilt Up By" "Angle" (Frame3D.tiltUpBy :: Angle -> Frame3D -> Frame3D) $(docs 'Frame3D.tiltUpBy)
    , Class.member1 "Tilt Down By" "Angle" (Frame3D.tiltDownBy :: Angle -> Frame3D -> Frame3D) $(docs 'Frame3D.tiltDownBy)
    , Class.member0 "Turn Right" (Frame3D.turnRight :: Frame3D -> Frame3D) $(docs 'Frame3D.turnRight)
    , Class.member0 "Turn Left" (Frame3D.turnLeft :: Frame3D -> Frame3D) $(docs 'Frame3D.turnLeft)
    , Class.member0 "Roll Right" (Frame3D.rollRight :: Frame3D -> Frame3D) $(docs 'Frame3D.rollRight)
    , Class.member0 "Roll Left" (Frame3D.rollLeft :: Frame3D -> Frame3D) $(docs 'Frame3D.rollLeft)
    , Class.member0 "Tilt Up" (Frame3D.tiltUp :: Frame3D -> Frame3D) $(docs 'Frame3D.tiltUp)
    , Class.member0 "Tilt Down" (Frame3D.tiltDown :: Frame3D -> Frame3D) $(docs 'Frame3D.tiltDown)
    ]
      <> rigidTransformations3D Frame3D.transformBy

type VectorCurve2D = VectorCurve2D.VectorCurve2D Unitless FFI.Space

vectorCurve2D :: Class
vectorCurve2D =
  Class.new @VectorCurve2D "A parametric curve defining a 2D unitless vector in terms of a parameter value." $
    [ Class.constant "Zero" (VectorCurve2D.zero :: VectorCurve2D) $(docs 'VectorCurve2D.zero)
    , Class.factory1 "Constant" "Value" VectorCurve2D.constant $(docs 'VectorCurve2D.constant)
    , Class.factory2 "XY" "X Component" "Y Component" VectorCurve2D.xy $(docs 'VectorCurve2D.xy)
    , Class.member1 "Evaluate" "Parameter Value" (flip VectorCurve2D.evaluate) $(docs 'VectorCurve2D.evaluate)
    ]

type DisplacementCurve2D = VectorCurve2D.VectorCurve2D Meters FFI.Space

displacementCurve2D :: Class
displacementCurve2D =
  Class.new @DisplacementCurve2D "A parametric curve defining a 2D displacement vector in terms of a parameter value." $
    [ Class.constant "Zero" (VectorCurve2D.zero :: DisplacementCurve2D) $(docs 'VectorCurve2D.zero)
    , Class.factory1 "Constant" "Value" VectorCurve2D.constant $(docs 'VectorCurve2D.constant)
    , Class.factory2 "XY" "X Component" "Y Component" VectorCurve2D.xy $(docs 'VectorCurve2D.xy)
    , Class.member1 "Evaluate" "Parameter Value" (flip VectorCurve2D.evaluate) $(docs 'VectorCurve2D.evaluate)
    ]

type UvVectorCurve = VectorCurve2D.VectorCurve2D Unitless UvSpace

uvVectorCurve :: Class
uvVectorCurve =
  Class.new @UvVectorCurve "A parametric vector curve in UV parameter space." $
    [ Class.constant "Zero" (VectorCurve2D.zero :: UvVectorCurve) $(docs 'VectorCurve2D.zero)
    , Class.factory1 "Constant" "Value" VectorCurve2D.constant $(docs 'VectorCurve2D.constant)
    , Class.factory2 "UV" "U Component" "V Component" VectorCurve2D.xy "Construct a UV vector curve from its U and V components."
    , Class.member1 "Evaluate" "Parameter Value" (flip VectorCurve2D.evaluate) $(docs 'VectorCurve2D.evaluate)
    ]

rigidTransformations2D ::
  FFI value =>
  (Transform2D.Rigid Meters FFI.Space -> value -> value) ->
  List (Class.Member value)
rigidTransformations2D transformBy =
  [ Class.member1 "Translate By" "Displacement" (Transform2D.translateByImpl transformBy) "Translate by the given displacement."
  , Class.member2 "Translate In" "Direction" "Distance" (Transform2D.translateInImpl transformBy) "Translate in the given direction by the given distance."
  , Class.member2 "Translate Along" "Axis" "Distance" (Transform2D.translateAlongImpl transformBy) "Translate along the given axis by the given distance."
  , Class.member2 "Rotate Around" "Point" "Angle" (Transform2D.rotateAroundImpl transformBy) "Rotate around the given point by the given angle."
  ]

orthonormalTransformations2D ::
  FFI value =>
  (forall tag. Transform.IsOrthonormal tag => Transform2D tag Meters FFI.Space -> value -> value) ->
  List (Class.Member value)
orthonormalTransformations2D transformBy =
  Class.member1 "Mirror Across" "Axis" (Transform2D.mirrorAcrossImpl transformBy) "Mirror across the given axis."
    : rigidTransformations2D transformBy

uniformTransformations2D ::
  FFI value =>
  (forall tag. Transform.IsUniform tag => Transform2D tag Meters FFI.Space -> value -> value) ->
  List (Class.Member value)
uniformTransformations2D transformBy =
  Class.member2 "Scale About" "Point" "Scale" (Transform2D.scaleAboutImpl transformBy) "Scale uniformly about the given point by the given scaling factor."
    : orthonormalTransformations2D transformBy

affineTransformations2D ::
  FFI value =>
  (forall tag. Transform2D tag Meters FFI.Space -> value -> value) ->
  List (Class.Member value)
affineTransformations2D transformBy =
  Class.member2 "Scale Along" "Axis" "Scale" (Transform2D.scaleAlongImpl transformBy) "Scale (stretch) along the given axis by the given scaling factor."
    : uniformTransformations2D transformBy

rigidTransformations3D ::
  FFI value =>
  (Transform3D.Rigid FFI.Space -> value -> value) ->
  List (Class.Member value)
rigidTransformations3D transformBy =
  [ Class.member1 "Translate By" "Displacement" (Transform3D.translateByImpl transformBy) "Translate by the given displacement."
  , Class.member2 "Translate In" "Direction" "Distance" (Transform3D.translateInImpl transformBy) "Translate in the given direction by the given distance."
  , Class.member2 "Translate Along" "Axis" "Distance" (Transform3D.translateAlongImpl transformBy) "Translate along the given axis by the given distance."
  , Class.member2 "Rotate Around" "Axis" "Angle" (Transform3D.rotateAroundImpl transformBy) "Rotate around the given axis by the given angle."
  ]

orthonormalTransformations3D ::
  forall value.
  FFI value =>
  (forall tag. Transform.IsOrthonormal tag => Transform3D tag FFI.Space -> value -> value) ->
  List (Class.Member value)
orthonormalTransformations3D transformBy =
  Class.member1 "Mirror Across" "Plane" (Transform3D.mirrorAcrossImpl transformBy :: Plane3D -> value -> value) "Mirror across the given plane."
    : rigidTransformations3D transformBy

uniformTransformations3D ::
  FFI value =>
  (forall tag. Transform.IsUniform tag => Transform3D tag FFI.Space -> value -> value) ->
  List (Class.Member value)
uniformTransformations3D transformBy =
  Class.member2 "Scale About" "Point" "Scale" (Transform3D.scaleAboutImpl transformBy) "Scale uniformly about the given point by the given scaling factor."
    : orthonormalTransformations3D transformBy

affineTransformations3D ::
  FFI value =>
  (forall tag. Transform3D tag FFI.Space -> value -> value) ->
  List (Class.Member value)
affineTransformations3D transformBy =
  Class.member2 "Scale Along" "Axis" "Scale" (Transform3D.scaleAlongImpl transformBy) "Scale (stretch) along the given axis by the given scaling factor."
    : uniformTransformations3D transformBy

type Curve2D = Curve2D.Curve2D Meters FFI.Space

curve2D :: Class
curve2D =
  Class.new @Curve2D $(docs ''Curve2D.Curve2D) $
    [ Class.factory1 "Constant" "Point" Curve2D.constant $(docs 'Curve2D.constant)
    , Class.factory2 "XY" "X Coordinate" "Y Coordinate" Curve2D.xy $(docs 'Curve2D.xy)
    , Class.factory1 "Line" "Line" Curve2D.line $(docs 'Curve2D.line)
    , Class.factory2 "Line From" "Start Point" "End Point" Curve2D.lineFrom $(docs 'Curve2D.lineFrom)
    , Class.factoryM3 "Arc From" "Start Point" "End Point" "Swept Angle" Curve2D.arcFrom $(docs 'Curve2D.arcFrom)
    , Class.factory4 "Polar Arc" "Center Point" "Radius" "Start Angle" "End Angle" Curve2D.polarArc $(docs 'Curve2D.polarArc)
    , Class.factory3 "Swept Arc" "Center Point" "Start Point" "Swept Angle" Curve2D.sweptArc $(docs 'Curve2D.sweptArc)
    , Class.factoryM4 "Corner Arc" "Corner Point" "Incoming" "Outgoing" "Radius" Curve2D.cornerArc $(docs 'Curve2D.cornerArc)
    , Class.factory1 "Circle" "Circle" Curve2D.circle $(docs 'Curve2D.circle)
    , Class.factory1 "Bezier" "Control Points" Curve2D.bezier $(docs 'Curve2D.bezier)
    , Class.factory4 "Hermite" "Start Point" "Start Derivatives" "End Point" "End Derivatives" Curve2D.hermite $(docs 'Curve2D.hermite)
    , Class.property "Start Point" (.startPoint) "The start point of the curve."
    , Class.property "End Point" (.endPoint) "The end point of the curve."
    , Class.member1 "Evaluate" "Parameter Value" (flip Curve2D.evaluate) $(docs 'Curve2D.evaluate)
    , Class.property "Derivative" Curve2D.derivative "The derivative of the curve."
    , Class.member0 "Reverse" Curve2D.reverse $(docs 'Curve2D.reverse)
    , Class.property "X Coordinate" (.xCoordinate) $(docs 'Curve2D.xCoordinate)
    , Class.property "Y Coordinate" (.yCoordinate) $(docs 'Curve2D.yCoordinate)
    , Class.plus @DisplacementCurve2D Self
    , Class.minus @DisplacementCurve2D Self
    , Class.minusSelf
    , Class.minus @Point2D Self
    ]
      <> affineTransformations2D Curve2D.transformBy

type UvCurve = Curve2D.Curve2D Unitless UvSpace

uvCurve :: Class
uvCurve =
  Class.new @UvCurve "A curve in UV parameter space." $
    [ Class.factory1 "Constant" "Point" Curve2D.constant $(docs 'Curve2D.constant)
    , Class.factory2 "UV" "U Coordinate" "V Coordinate" Curve2D.xy $(docs 'Curve2D.xy)
    , Class.factory1 "Line" "Line" Curve2D.line $(docs 'Curve2D.line)
    , Class.factory2 "Line From" "Start Point" "End Point" Curve2D.lineFrom $(docs 'Curve2D.lineFrom)
    , Class.factoryU3 "Arc From" "Start Point" "End Point" "Swept Angle" Curve2D.arcFrom $(docs 'Curve2D.arcFrom)
    , Class.factory4 "Polar Arc" "Center Point" "Radius" "Start Angle" "End Angle" Curve2D.polarArc $(docs 'Curve2D.polarArc)
    , Class.factory1 "Circle" "Circle" Curve2D.circle $(docs 'Curve2D.circle)
    , Class.factory3 "Swept Arc" "Center Point" "Start Point" "Swept Angle" Curve2D.sweptArc $(docs 'Curve2D.sweptArc)
    , Class.factoryU4 "Corner Arc" "Corner Point" "Incoming" "Outgoing" "Radius" Curve2D.cornerArc $(docs 'Curve2D.cornerArc)
    , Class.factory1 "Bezier" "Control Points" Curve2D.bezier $(docs 'Curve2D.bezier)
    , Class.factory4 "Hermite" "Start Point" "Start Derivatives" "End Point" "End Derivatives" Curve2D.hermite $(docs 'Curve2D.hermite)
    , Class.property "Start Point" (.startPoint) "The start point of the curve."
    , Class.property "End Point" (.endPoint) "The end point of the curve."
    , Class.member1 "Evaluate" "Parameter Value" (flip Curve2D.evaluate) $(docs 'Curve2D.evaluate)
    , Class.property "Derivative" Curve2D.derivative "The derivative of the curve."
    , Class.member0 "Reverse" Curve2D.reverse $(docs 'Curve2D.reverse)
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

type Region2D = Region2D.Region2D Meters FFI.Space

region2D :: Class
region2D =
  Class.new @Region2D $(docs ''Region2D.Region2D) $
    [ Class.factoryM1R "Bounded By" "Curves" Region2D.boundedBy $(docs 'Region2D.boundedBy)
    , Class.factoryM1R "Rectangle" "Bounding Box" Region2D.rectangle $(docs 'Region2D.rectangle)
    , Class.factoryM1R "Circle" "Circle" Region2D.circle $(docs 'Region2D.circle)
    , Class.property "Outer Loop" Region2D.outerLoop region2dOuterLoopDocs
    , Class.property "Inner Loops" Region2D.innerLoops region2dInnerLoopsDocs
    , Class.property "Boundary Curves" Region2D.boundaryCurves region2dBoundaryCurvesDocs
    , Class.factoryM1R "Polygon" "Polygon" Region2D.polygon $(docs 'Region2D.polygon)
    , Class.memberM2 "Fillet" "Points" "Radius" Region2D.fillet $(docs 'Region2D.fillet)
    ]
      <> affineTransformations2D Region2D.transformBy

type UvRegion = Region2D.Region2D Unitless UvSpace

uvRegion :: Class
uvRegion =
  Class.new @UvRegion "A region in UV parameter space." $
    [ Class.constant "Unit Square" Region2D.unitSquare $(docs 'Region2D.unitSquare)
    , Class.factoryU1R "Bounded By" "Curves" Region2D.boundedBy $(docs 'Region2D.boundedBy)
    , Class.factoryU1R "Rectangle" "Bounding Box" Region2D.rectangle $(docs 'Region2D.rectangle)
    , Class.factoryU1R "Circle" "Circle" Region2D.circle $(docs 'Region2D.circle)
    , Class.property "Outer Loop" Region2D.outerLoop region2dOuterLoopDocs
    , Class.property "Inner Loops" Region2D.innerLoops region2dInnerLoopsDocs
    , Class.property "Boundary Curves" Region2D.boundaryCurves region2dBoundaryCurvesDocs
    ]

type Body3D = Body3D.Body3D FFI.Space

body3D :: Class
body3D = do
  let writeStl path convention givenResolution body = do
        let mesh = Body3D.toPointMesh givenResolution body
        Stl.writeBinary path convention Length.inMillimeters mesh
  let writeMitsuba path givenResolution body = do
        let mesh = Body3D.toSurfaceMesh givenResolution body
        Mitsuba.writeMeshes path [(mesh, #name "")]
  Class.new @Body3D $(docs ''Body3D.Body3D) $
    [ Class.factoryM4R "Extruded" "Sketch Plane" "Profile" "Start" "End" (Body3D.extruded @FFI.Space @FFI.Space) $(docs 'Body3D.extruded)
    , Class.factoryM4R "Revolved" "Sketch Plane" "Profile" "Axis" "Angle" (Body3D.revolved @FFI.Space @FFI.Space) $(docs 'Body3D.revolved)
    , Class.factoryM1R "Block" "Bounding Box" Body3D.block $(docs 'Body3D.block)
    , Class.factoryM2R "Sphere" "Center Point" "Diameter" Body3D.sphere $(docs 'Body3D.sphere)
    , Class.factoryM3R "Cylinder" "Start Point" "End Point" "Diameter" Body3D.cylinder $(docs 'Body3D.cylinder)
    , Class.factoryM4R "Cylinder Along" "Axis" "Start" "End" "Diameter" Body3D.cylinderAlong $(docs 'Body3D.cylinderAlong)
    , Class.member1 "Place In" "Frame" (Body3D.placeIn :: Frame3D -> Body3D -> Body3D) $(docs 'Body3D.placeIn)
    , Class.member1 "Relative To" "Frame" (Body3D.relativeTo :: Frame3D -> Body3D -> Body3D) $(docs 'Body3D.relativeTo)
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

type Model3D = Model3D.Model3D FFI.Space

model3D :: Class
model3D =
  Class.new @Model3D $(docs ''Model3D.Model3D) $
    [ Class.constant "Nothing" (Model3D.nothing :: Model3D) $(docs 'Model3D.nothing)
    , Class.factoryM1 "Body" "Body" Model3D.body $(docs 'Model3D.body)
    , Class.factoryM2 "Body With" "Attributes" "Body" Model3D.bodyWith $(docs 'Model3D.bodyWith)
    , Class.factory1 "Group" "Children" Model3D.group $(docs 'Model3D.group)
    , Class.factory2 "Group With" "Attributes" "Children" Model3D.groupWith $(docs 'Model3D.groupWith)
    , Class.member1 "With Name" "Name" Model3D.withName $(docs 'Model3D.withName)
    , Class.member1 "With PBR Material" "Material" Model3D.withPbrMaterial $(docs 'Model3D.withPbrMaterial)
    , Class.member1 "With Opacity" "Opacity" Model3D.withOpacity $(docs 'Model3D.withOpacity)
    , Class.member1 "With Attributes" "Attributes" Model3D.withAttributes $(docs 'Model3D.withAttributes)
    , Class.nested @Model3D.Attribute $(docs ''Model3D.Attribute) []
    , Class.static1 "Name" "Name" Model3D.name $(docs 'Model3D.name)
    , Class.static1 "PBR Material" "Material" Model3D.pbrMaterial $(docs 'Model3D.pbrMaterial)
    , Class.static1 "Opacity" "Opacity" Model3D.opacity $(docs 'Model3D.opacity)
    , Class.member1 "Place In" "Frame" (Model3D.placeIn :: Frame3D -> Model3D -> Model3D) $(docs 'Model3D.placeIn)
    , Class.member1 "Relative To" "Frame" (Model3D.relativeTo :: Frame3D -> Model3D -> Model3D) $(docs 'Model3D.relativeTo)
    ]
      <> rigidTransformations3D Model3D.transformBy

newtype Gltf = Gltf Model3D

instance FFI Gltf where
  representation = FFI.classRepresentation "Gltf"

gltf :: Class
gltf = do
  let writeBinary path res (Gltf model) = Gltf.writeBinary path model res
  Class.new @Gltf "A glTF model that can be written out to a file." $
    [ Class.constructor1 "Model" Gltf "Construct a glTF model from a generic 3D model."
    , Class.member2 "Write Binary" "Path" "Resolution" writeBinary $(docs 'Gltf.writeBinary)
    ]

type Camera3D = Camera3D.Camera3D FFI.Space

camera3D :: Class
camera3D =
  Class.new @Camera3D $(docs ''Camera3D.Camera3D) $
    [ Class.factory3 "Look At" "Eye Point" "Focal Point" "Projection" Camera3D.lookAt $(docs 'Camera3D.lookAt)
    , Class.factory5 "Orbit" "Focal Point" "Azimuth" "Elevation" "Distance" "Projection" Camera3D.orbit $(docs 'Camera3D.orbit)
    , Class.nested @Camera3D.Projection $(docs ''Camera3D.Projection) []
    , Class.static1 "Perspective" "Vertical FOV" Camera3D.perspective $(docs 'Camera3D.perspective)
    , Class.static1 "Orthographic" "Viewport Height" Camera3D.orthographic $(docs 'Camera3D.orthographic)
    ]

data Mitsuba = Mitsuba Model3D Camera3D (Mitsuba.Lighting FFI.Space)

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
    , Class.static2 "Environment Map" "Frame" "Image" (Mitsuba.environmentMap :: Frame3D -> Text -> Mitsuba.Lighting FFI.Space) $(docs 'Mitsuba.environmentMap)
    ]

spurGear :: Class
spurGear =
  Class.new @SpurGear $(docs ''SpurGear) $
    [ Class.factory2 "Metric" "Num Teeth" "Module" SpurGear.metric $(docs 'SpurGear.metric)
    , Class.property "Num Teeth" (.numTeeth) "The number of teeth of a gear."
    , Class.property "Module" (.module_) "The module of a gear."
    , Class.property "Pitch Diameter" (.pitchDiameter) "The pitch diameter of a gear."
    , Class.property "Outer Diameter" (.outerDiameter) "The outer diameter of a gear."
    , Class.memberM0 "Profile" (SpurGear.profile :: SpurGear -> List Curve2D) $(docs 'SpurGear.profile)
    ]
