module API (classes, functions) where

import API.Class
  ( Class
  , Self (Self)
  , absSelf
  , comparison
  , constant
  , constructor2
  , crossProduct
  , crossSelf
  , curry1T2
  , curry1T3
  , curryT2
  , curryT4
  , divBy
  , divByFloat
  , divBySelf
  , dotProduct
  , dotSelf
  , equality
  , factory1
  , factory2
  , factory3
  , factory4
  , factoryM1R
  , factoryM2R
  , factoryM3
  , factoryM3R
  , factoryM4
  , factoryM4R
  , factoryU1R
  , factoryU2R
  , factoryU3
  , factoryU4
  , floatDivBy
  , floatMinus
  , floatPlus
  , floatTimes
  , floorDivBySelf
  , member0
  , member1
  , member2
  , memberM0
  , memberM2
  , memberM3
  , memberR0
  , memberS0
  , memberU0
  , minus
  , minusFloat
  , minusSelf
  , modBySelf
  , negateSelf
  , nested
  , plus
  , plusFloat
  , plusSelf
  , property
  , static1
  , static3
  , times
  , timesFloat
  , timesSelf
  , upcast
  )
import API.Class qualified as Class
import API.Docs (docs)
import API.Function (Function)
import API.Space (Space)
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
import OpenSolid.Frame3d (Frame3d)
import OpenSolid.Frame3d qualified as Frame3d
import OpenSolid.Length (Length)
import OpenSolid.Length qualified as Length
import OpenSolid.List qualified as List
import OpenSolid.Mitsuba qualified as Mitsuba
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
import OpenSolid.Polygon2d (Polygon2d)
import OpenSolid.Polygon2d qualified as Polygon2d
import OpenSolid.Prelude
import OpenSolid.Qty qualified as Qty
import OpenSolid.Region2d (Region2d)
import OpenSolid.Region2d qualified as Region2d
import OpenSolid.Resolution (Resolution (Resolution))
import OpenSolid.Resolution qualified as Resolution
import OpenSolid.Scene3d (Scene3d)
import OpenSolid.Scene3d qualified as Scene3d
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
  , polygon2d
  , body3d
  , resolution
  , pbrMaterial
  , scene3d
  , spurGear
  ]

functions :: List Function
functions = List.collect Class.functions classes

length :: Class
length =
  Class.new @Length $(docs ''Length) $
    [ constant "Zero" Length.zero $(docs 'Length.zero)
    , factory3 "Interpolate" "Start" "End" "Parameter Value" Qty.interpolateFrom $(docs 'Qty.interpolateFrom)
    , static3 "Steps" "Start" "End" "N" (Qty.steps @Meters) $(docs 'Qty.steps)
    , static3 "Leading" "Start" "End" "N" (Qty.leading @Meters) $(docs 'Qty.leading)
    , static3 "Trailing" "Start" "End" "N" (Qty.trailing @Meters) $(docs 'Qty.trailing)
    , static3 "In Between" "Start" "End" "N" (Qty.inBetween @Meters) $(docs 'Qty.inBetween)
    , static3 "Midpoints" "Start" "End" "N" (Qty.midpoints @Meters) $(docs 'Qty.midpoints)
    , factory1 "Meters" "Value" Length.meters $(docs 'Length.meters)
    , factory1 "Centimeters" "Value" Length.centimeters $(docs 'Length.centimeters)
    , factory1 "Cm" "Value" Length.cm $(docs 'Length.cm)
    , factory1 "Millimeters" "Value" Length.millimeters $(docs 'Length.millimeters)
    , factory1 "Mm" "Value" Length.mm $(docs 'Length.mm)
    , factory1 "Micrometers" "Value" Length.micrometers $(docs 'Length.micrometers)
    , factory1 "Nanometers" "Value" Length.nanometers $(docs 'Length.nanometers)
    , factory1 "Inches" "Value" Length.inches $(docs 'Length.inches)
    , factory1 "Pixels" "Value" Length.pixels $(docs 'Length.pixels)
    , member0 "In Meters" Length.inMeters $(docs 'Length.inMeters)
    , member0 "In Centimeters" Length.inCentimeters $(docs 'Length.inCentimeters)
    , member0 "In Millimeters" Length.inMillimeters $(docs 'Length.inMillimeters)
    , member0 "In Micrometers" Length.inMicrometers $(docs 'Length.inMicrometers)
    , member0 "In Nanometers" Length.inNanometers $(docs 'Length.inNanometers)
    , member0 "In Inches" Length.inInches $(docs 'Length.inInches)
    , member0 "In Pixels" Length.inPixels $(docs 'Length.inPixels)
    , memberM0 "Is Zero" (~= Length.zero) "Check if a length is zero, within the current tolerance."
    , equality
    , comparison
    , negateSelf
    , absSelf Qty.abs
    , floatTimes
    , plusSelf
    , plus @(Bounds Meters) Self
    , plus @(Curve Meters) Self
    , minusSelf
    , minus @(Bounds Meters) Self
    , minus @(Curve Meters) Self
    , timesFloat
    , timesSelf
    , times @(Bounds Unitless) Self
    , times @(Bounds Meters) Self
    , times @(Curve Unitless) Self
    , times @(Curve Meters) Self
    , times @(Direction2d Space) Self
    , times @(Vector2d (Space @ Unitless)) Self
    , times @(Vector2d (Space @ Meters)) Self
    , divByFloat
    , divBySelf
    , divBy @(Bounds Unitless) Self
    , divBy @(Bounds Meters) Self
    , divBy @(Curve Unitless) Self
    , divBy @(Curve Meters) Self
    , floorDivBySelf
    , modBySelf
    ]

area :: Class
area =
  Class.new @Area $(docs ''Area) $
    [ constant "Zero" Area.zero $(docs 'Area.zero)
    , factory3 "Interpolate" "Start" "End" "Parameter Value" Qty.interpolateFrom $(docs 'Qty.interpolateFrom)
    , static3 "Steps" "Start" "End" "N" (Qty.steps @SquareMeters) $(docs 'Qty.steps)
    , static3 "Leading" "Start" "End" "N" (Qty.leading @SquareMeters) $(docs 'Qty.leading)
    , static3 "Trailing" "Start" "End" "N" (Qty.trailing @SquareMeters) $(docs 'Qty.trailing)
    , static3 "In Between" "Start" "End" "N" (Qty.inBetween @SquareMeters) $(docs 'Qty.inBetween)
    , static3 "Midpoints" "Start" "End" "N" (Qty.midpoints @SquareMeters) $(docs 'Qty.midpoints)
    , factory1 "Square Meters" "Value" Area.squareMeters $(docs 'Area.squareMeters)
    , factory1 "Square Inches" "Value" Area.squareInches $(docs 'Area.squareInches)
    , member0 "In Square Meters" Area.inSquareMeters $(docs 'Area.inSquareMeters)
    , member0 "In Square Inches" Area.inSquareInches $(docs 'Area.inSquareInches)
    , memberS0 "Is Zero" (~= Area.zero) "Check if an area is zero, within the current tolerance."
    , equality
    , comparison
    , negateSelf
    , absSelf Qty.abs
    , floatTimes
    , plusSelf
    , plus @(Bounds SquareMeters) Self
    , plus @(Curve SquareMeters) Self
    , minusSelf
    , minus @(Bounds SquareMeters) Self
    , minus @(Curve SquareMeters) Self
    , timesFloat
    , times @(Bounds Unitless) Self
    , times @(Curve Unitless) Self
    , times @(Direction2d Space) Self
    , times @(Vector2d (Space @ Unitless)) Self
    , divByFloat
    , divBySelf
    , divBy @Length Self
    , divBy @(Bounds Unitless) Self
    , divBy @(Bounds Meters) Self
    , divBy @(Bounds SquareMeters) Self
    , divBy @(Curve Unitless) Self
    , divBy @(Curve Meters) Self
    , divBy @(Curve SquareMeters) Self
    , floorDivBySelf
    , modBySelf
    ]

angle :: Class
angle =
  Class.new @Angle $(docs ''Angle) $
    [ constant "Zero" Angle.zero $(docs 'Angle.zero)
    , factory3 "Interpolate" "Start" "End" "Parameter Value" Qty.interpolateFrom $(docs 'Qty.interpolateFrom)
    , static3 "Steps" "Start" "End" "N" (Qty.steps @Radians) $(docs 'Qty.steps)
    , static3 "Leading" "Start" "End" "N" (Qty.leading @Radians) $(docs 'Qty.leading)
    , static3 "Trailing" "Start" "End" "N" (Qty.trailing @Radians) $(docs 'Qty.trailing)
    , static3 "In Between" "Start" "End" "N" (Qty.inBetween @Radians) $(docs 'Qty.inBetween)
    , static3 "Midpoints" "Start" "End" "N" (Qty.midpoints @Radians) $(docs 'Qty.midpoints)
    , constant "Golden Angle" Angle.goldenAngle $(docs 'Angle.goldenAngle)
    , constant "Radian" Angle.radian $(docs 'Angle.radian)
    , constant "Full Turn" Angle.fullTurn $(docs 'Angle.fullTurn)
    , constant "Half Turn" Angle.halfTurn $(docs 'Angle.halfTurn)
    , constant "Quarter Turn" Angle.quarterTurn $(docs 'Angle.quarterTurn)
    , constant "Pi" Angle.pi $(docs 'Angle.pi)
    , constant "Two Pi" Angle.twoPi $(docs 'Angle.twoPi)
    , factory1 "Radians" "Value" Angle.radians $(docs 'Angle.radians)
    , factory1 "Degrees" "Value" Angle.degrees $(docs 'Angle.degrees)
    , factory1 "Turns" "Value" Angle.turns $(docs 'Angle.turns)
    , factory1 "Acos" "Value" Angle.acos $(docs 'Angle.acos)
    , factory1 "Asin" "Value" Angle.asin $(docs 'Angle.asin)
    , factory1 "Atan" "Value" Angle.atan $(docs 'Angle.atan)
    , member0 "In Radians" Angle.inRadians $(docs 'Angle.inRadians)
    , member0 "In Degrees" Angle.inDegrees $(docs 'Angle.inDegrees)
    , member0 "In Turns" Angle.inTurns $(docs 'Angle.inTurns)
    , memberR0 "Is Zero" (~= Angle.zero) "Check if an angle is zero, within the current tolerance."
    , member0 "Sin" Angle.sin $(docs 'Angle.sin)
    , member0 "Cos" Angle.cos $(docs 'Angle.cos)
    , member0 "Tan" Angle.tan $(docs 'Angle.tan)
    , equality
    , comparison
    , negateSelf
    , absSelf Qty.abs
    , floatTimes
    , plusSelf
    , plus @(Bounds Radians) Self
    , plus @(Curve Radians) Self
    , minusSelf
    , minus @(Bounds Radians) Self
    , minus @(Curve Radians) Self
    , timesFloat
    , times @(Bounds Unitless) Self
    , times @(Curve Unitless) Self
    , divByFloat
    , divBySelf
    , divBy @(Bounds Unitless) Self
    , divBy @(Bounds Radians) Self
    , divBy @(Curve Unitless) Self
    , divBy @(Curve Radians) Self
    , floorDivBySelf
    , modBySelf
    ]

bounds :: Class
bounds =
  Class.new @(Bounds Unitless) "A range of unitless values, with a lower bound and upper bound." $
    [ constant "Unit Interval" Bounds.unitInterval $(docs 'Bounds.unitInterval)
    , constructor2 "First Value" "Second Value" Bounds $(docs 'Bounds)
    , factory1 "Constant" "Value" Bounds.constant $(docs 'Bounds.constant)
    , factory1 "Zero To" "Value" Bounds.zeroTo $(docs 'Bounds.zeroTo)
    , factory1 "Symmetric" "Width" Bounds.symmetric $(docs 'Bounds.symmetric)
    , factory1 "Hull" "Values" Bounds.hullN $(docs 'Bounds.hullN)
    , factory1 "Aggregate" "Bounds" Bounds.aggregateN $(docs 'Bounds.aggregateN)
    , property "Endpoints" (.endpoints) $(docs 'Bounds.endpoints)
    , property "Lower" (.lower) $(docs 'Bounds.lower)
    , property "Upper" (.upper) $(docs 'Bounds.upper)
    , member1 "Intersection" "Other" Bounds.intersection $(docs 'Bounds.intersection)
    , member1 "Includes" "Value" Bounds.includes $(docs 'Bounds.includes)
    , member1 "Contains" "Other" Bounds.contains $(docs 'Bounds.contains)
    , negateSelf
    , absSelf Bounds.abs
    , floatPlus
    , floatMinus
    , floatTimes
    , floatDivBy
    , plusFloat
    , plusSelf
    , minusFloat
    , minusSelf
    , timesFloat
    , timesSelf
    , times @Length Self
    , times @Area Self
    , times @Angle Self
    , times @(Bounds Meters) Self
    , times @(Bounds SquareMeters) Self
    , times @(Bounds Radians) Self
    , divByFloat
    , divBySelf
    ]

lengthBounds :: Class
lengthBounds =
  Class.new @(Bounds Meters) "A range of length values, with a lower bound and upper bound." $
    [ constructor2 "First Value" "Second Value" Bounds $(docs 'Bounds)
    , factory1 "Constant" "Value" Bounds.constant $(docs 'Bounds.constant)
    , factory1 "Zero To" "Value" Bounds.zeroTo $(docs 'Bounds.zeroTo)
    , factory1 "Symmetric" "Width" Bounds.symmetric $(docs 'Bounds.symmetric)
    , factory1 "Hull" "Values" Bounds.hullN $(docs 'Bounds.hullN)
    , factory1 "Aggregate" "Bounds" Bounds.aggregateN $(docs 'Bounds.aggregateN)
    , property "Endpoints" (.endpoints) $(docs 'Bounds.endpoints)
    , property "Lower" (.lower) $(docs 'Bounds.lower)
    , property "Upper" (.upper) $(docs 'Bounds.upper)
    , member1 "Intersection" "Other" Bounds.intersection $(docs 'Bounds.intersection)
    , member1 "Includes" "Value" Bounds.includes $(docs 'Bounds.includes)
    , member1 "Contains" "Other" Bounds.contains $(docs 'Bounds.contains)
    , negateSelf
    , absSelf Bounds.abs
    , floatTimes
    , plusSelf
    , plus @Length Self
    , minusSelf
    , minus @Length Self
    , timesFloat
    , timesSelf
    , times @Length Self
    , times @(Bounds Unitless) Self
    , divByFloat
    , divBySelf
    , divBy @Length Self
    , divBy @(Bounds Unitless) Self
    ]

areaBounds :: Class
areaBounds =
  Class.new @(Bounds SquareMeters) "A range of area values, with a lower bound and upper bound." $
    [ constructor2 "First Value" "Second Value" Bounds $(docs 'Bounds)
    , factory1 "Constant" "Value" Bounds.constant $(docs 'Bounds.constant)
    , factory1 "Zero To" "Value" Bounds.zeroTo $(docs 'Bounds.zeroTo)
    , factory1 "Symmetric" "Width" Bounds.symmetric $(docs 'Bounds.symmetric)
    , factory1 "Hull" "Values" Bounds.hullN $(docs 'Bounds.hullN)
    , factory1 "Aggregate" "Bounds" Bounds.aggregateN $(docs 'Bounds.aggregateN)
    , property "Endpoints" (.endpoints) $(docs 'Bounds.endpoints)
    , property "Lower" (.lower) $(docs 'Bounds.lower)
    , property "Upper" (.upper) $(docs 'Bounds.upper)
    , member1 "Intersection" "Other" Bounds.intersection $(docs 'Bounds.intersection)
    , member1 "Includes" "Value" Bounds.includes $(docs 'Bounds.includes)
    , member1 "Contains" "Other" Bounds.contains $(docs 'Bounds.contains)
    , negateSelf
    , absSelf Bounds.abs
    , floatTimes
    , plusSelf
    , plus @Area Self
    , minusSelf
    , minus @Area Self
    , timesFloat
    , times @(Bounds Unitless) Self
    , divByFloat
    , divBySelf
    , divBy @Length Self
    , divBy @Area Self
    , divBy @(Bounds Unitless) Self
    , divBy @(Bounds Meters) Self
    ]

angleBounds :: Class
angleBounds =
  Class.new @(Bounds Radians) "A range of angle values, with a lower bound and upper bound." $
    [ constructor2 "First Value" "Second Value" Bounds $(docs 'Bounds)
    , factory1 "Constant" "Value" Bounds.constant $(docs 'Bounds.constant)
    , factory1 "Zero To" "Value" Bounds.zeroTo $(docs 'Bounds.zeroTo)
    , factory1 "Symmetric" "Width" Bounds.symmetric $(docs 'Bounds.symmetric)
    , factory1 "Hull" "Values" Bounds.hullN $(docs 'Bounds.hullN)
    , factory1 "Aggregate" "Bounds" Bounds.aggregateN $(docs 'Bounds.aggregateN)
    , property "Endpoints" (.endpoints) $(docs 'Bounds.endpoints)
    , property "Lower" (.lower) $(docs 'Bounds.lower)
    , property "Upper" (.upper) $(docs 'Bounds.upper)
    , member1 "Intersection" "Other" Bounds.intersection $(docs 'Bounds.intersection)
    , member1 "Includes" "Value" Bounds.includes $(docs 'Bounds.includes)
    , member1 "Contains" "Other" Bounds.contains $(docs 'Bounds.contains)
    , negateSelf
    , absSelf Bounds.abs
    , floatTimes
    , plusSelf
    , plus @Angle Self
    , minusSelf
    , minus @Angle Self
    , timesFloat
    , times @(Bounds Unitless) Self
    , divByFloat
    , divBySelf
    , divBy @Angle Self
    , divBy @(Bounds Unitless) Self
    ]

color :: Class
color =
  Class.new @Color $(docs ''Color) $
    [ factory3 "RGB Float" "Red" "Green" "Blue" Color.rgbFloat $(docs 'Color.rgbFloat)
    , factory3 "RGB Int" "Red" "Green" "Blue" Color.rgbInt $(docs 'Color.rgbInt)
    , factory3 "HSL" "Hue" "Saturation" "Lightness" Color.hsl $(docs 'Color.hsl)
    , factory1 "From Hex" "Hex String" Color.fromHex $(docs 'Color.fromHex)
    , member0 "To Hex" Color.toHex $(docs 'Color.toHex)
    , property "RGB Float Components" (.rgbFloatComponents) $(docs 'Color.rgbFloatComponents)
    , property "RGB Int Components" (.rgbIntComponents) $(docs 'Color.rgbIntComponents)
    , constant "Red" Color.red $(docs 'Color.red)
    , constant "Dark Red" Color.darkRed $(docs 'Color.darkRed)
    , constant "Light Orange" Color.lightOrange $(docs 'Color.lightOrange)
    , constant "Orange" Color.orange $(docs 'Color.orange)
    , constant "Dark Orange" Color.darkOrange $(docs 'Color.darkOrange)
    , constant "Light Yellow" Color.lightYellow $(docs 'Color.lightYellow)
    , constant "Yellow" Color.yellow $(docs 'Color.yellow)
    , constant "Dark Yellow" Color.darkYellow $(docs 'Color.darkYellow)
    , constant "Light Green" Color.lightGreen $(docs 'Color.lightGreen)
    , constant "Green" Color.green $(docs 'Color.green)
    , constant "Dark Green" Color.darkGreen $(docs 'Color.darkGreen)
    , constant "Light Blue" Color.lightBlue $(docs 'Color.lightBlue)
    , constant "Blue" Color.blue $(docs 'Color.blue)
    , constant "Dark Blue" Color.darkBlue $(docs 'Color.darkBlue)
    , constant "Light Purple" Color.lightPurple $(docs 'Color.lightPurple)
    , constant "Purple" Color.purple $(docs 'Color.purple)
    , constant "Dark Purple" Color.darkPurple $(docs 'Color.darkPurple)
    , constant "Light Brown" Color.lightBrown $(docs 'Color.lightBrown)
    , constant "Brown" Color.brown $(docs 'Color.brown)
    , constant "Dark Brown" Color.darkBrown $(docs 'Color.darkBrown)
    , constant "Black" Color.black $(docs 'Color.black)
    , constant "White" Color.white $(docs 'Color.white)
    , constant "Light Grey" Color.lightGrey $(docs 'Color.lightGrey)
    , constant "Grey" Color.grey $(docs 'Color.grey)
    , constant "Dark Grey" Color.darkGrey $(docs 'Color.darkGrey)
    , constant "Light Gray" Color.lightGray $(docs 'Color.lightGray)
    , constant "Gray" Color.gray $(docs 'Color.gray)
    , constant "Dark Gray" Color.darkGray $(docs 'Color.darkGray)
    , constant "Light Charcoal" Color.lightCharcoal $(docs 'Color.lightCharcoal)
    , constant "Charcoal" Color.charcoal $(docs 'Color.charcoal)
    , constant "Dark Charcoal" Color.darkCharcoal $(docs 'Color.darkCharcoal)
    ]

vector2d :: Class
vector2d =
  Class.new @(Vector2d (Space @ Unitless)) "A unitless vector in 2D." $
    [ constant "Zero" (Vector2d.zero @Space @Unitless) $(docs 'Vector2d.zero)
    , factory1 "Unit" "Direction" Vector2d.unit $(docs 'Vector2d.unit)
    , constructor2 "X Component" "Y Component" Vector2d $(docs 'Vector2d)
    , factory1 "Y" "Y Component" Vector2d.y $(docs 'Vector2d.y)
    , factory1 "X" "X Component" Vector2d.x $(docs 'Vector2d.x)
    , factory2 "Polar" "Magnitude" "Angle" Vector2d.polar $(docs 'Vector2d.polar)
    , factory1 "From Components" "Components" Vector2d.fromComponents $(docs 'Vector2d.fromComponents)
    , property "Components" (.components) $(docs 'Vector2d.components)
    , property "X Component" (.xComponent) $(docs 'Vector2d.xComponent)
    , property "Y Component" (.yComponent) $(docs 'Vector2d.yComponent)
    , memberU0 "Direction" Vector2d.direction $(docs 'Vector2d.direction)
    , member0 "Normalize" Vector2d.normalize $(docs 'Vector2d.normalize)
    , property "Angle" (.angle) $(docs 'Vector2d.angle)
    , member1 "Angle To" "Other" (flip Vector2d.angleFrom) $(docs 'Vector2d.angleFrom)
    , memberU0 "Is Zero" (~= Vector2d.zero) "Check if a vector is zero, within the current tolerance."
    , negateSelf
    , floatTimes
    , plusSelf
    , minusSelf
    , timesFloat
    , times @Length Self
    , times @Area Self
    , divByFloat
    , dotSelf
    , dotProduct @(Vector2d (Space @ Meters)) Self
    , dotProduct @(Vector2d (Space @ SquareMeters)) Self
    , crossSelf
    , crossProduct @(Vector2d (Space @ Meters)) Self
    , crossProduct @(Vector2d (Space @ SquareMeters)) Self
    ]

displacement2d :: Class
displacement2d =
  Class.new @(Vector2d (Space @ Meters)) "A displacement vector in 2D." $
    [ constant "Zero" (Vector2d.zero @Space @Meters) $(docs 'Vector2d.zero)
    , constructor2 "X Component" "Y Component" Vector2d $(docs 'Vector2d)
    , factory1 "X" "X Component" Vector2d.x $(docs 'Vector2d.x)
    , factory1 "Y" "Y Component" Vector2d.y $(docs 'Vector2d.y)
    , factory2 "Polar" "Magnitude" "Angle" Vector2d.polar $(docs 'Vector2d.polar)
    , factory2 "Meters" "X Component" "Y Component" Vector2d.meters $(docs 'Vector2d.meters)
    , factory2 "Centimeters" "X Component" "Y Component" Vector2d.centimeters $(docs 'Vector2d.centimeters)
    , factory2 "Cm" "X Component" "Y Component" Vector2d.cm $(docs 'Vector2d.cm)
    , factory2 "Millimeters" "X Component" "Y Component" Vector2d.millimeters $(docs 'Vector2d.millimeters)
    , factory2 "Mm" "X Component" "Y Component" Vector2d.mm $(docs 'Vector2d.mm)
    , factory2 "Inches" "X Component" "Y Component" Vector2d.inches $(docs 'Vector2d.inches)
    , factory1 "From Components" "Components" Vector2d.fromComponents $(docs 'Vector2d.fromComponents)
    , property "Components" (.components) $(docs 'Vector2d.components)
    , property "X Component" (.xComponent) $(docs 'Vector2d.xComponent)
    , property "Y Component" (.yComponent) $(docs 'Vector2d.yComponent)
    , memberM0 "Direction" Vector2d.direction $(docs 'Vector2d.direction)
    , member0 "Normalize" Vector2d.normalize $(docs 'Vector2d.normalize)
    , property "Angle" (.angle) $(docs 'Vector2d.angle)
    , member1 "Angle To" "Other" (flip Vector2d.angleFrom) $(docs 'Vector2d.angleFrom)
    , memberM0 "Is Zero" (~= Vector2d.zero) "Check if a displacement is zero, within the current tolerance."
    , negateSelf
    , floatTimes
    , plusSelf
    , minusSelf
    , timesFloat
    , times @Length Self
    , divByFloat
    , divBy @Length Self
    , dotSelf
    , dotProduct @(Vector2d (Space @ Unitless)) Self
    , crossSelf
    , crossProduct @(Vector2d (Space @ Unitless)) Self
    ]

areaVector2d :: Class
areaVector2d =
  Class.new @(Vector2d (Space @ SquareMeters)) "A vector in 2D with units of area." $
    [ constant "Zero" (Vector2d.zero @Space @SquareMeters) $(docs 'Vector2d.zero)
    , constructor2 "X Component" "Y Component" Vector2d $(docs 'Vector2d)
    , factory1 "X" "X Component" Vector2d.x $(docs 'Vector2d.x)
    , factory1 "Y" "Y Component" Vector2d.y $(docs 'Vector2d.y)
    , factory2 "Polar" "Magnitude" "Angle" Vector2d.polar $(docs 'Vector2d.polar)
    , factory2 "Square Meters" "X Component" "Y Component" Vector2d.squareMeters $(docs 'Vector2d.squareMeters)
    , factory1 "From Components" "Components" Vector2d.fromComponents $(docs 'Vector2d.fromComponents)
    , property "Components" (.components) $(docs 'Vector2d.components)
    , property "X Component" (.xComponent) $(docs 'Vector2d.xComponent)
    , property "Y Component" (.yComponent) $(docs 'Vector2d.yComponent)
    , memberS0 "Direction" Vector2d.direction $(docs 'Vector2d.direction)
    , member0 "Normalize" Vector2d.normalize $(docs 'Vector2d.normalize)
    , property "Angle" (.angle) $(docs 'Vector2d.angle)
    , member1 "Angle To" "Other" (flip Vector2d.angleFrom) $(docs 'Vector2d.angleFrom)
    , memberS0 "Is Zero" (~= Vector2d.zero) "Check if an area vector is zero, within the current tolerance."
    , negateSelf
    , floatTimes
    , plusSelf
    , minusSelf
    , timesFloat
    , divByFloat
    , divBy @Length Self
    , divBy @Area Self
    , dotProduct @(Vector2d (Space @ Unitless)) Self
    , crossProduct @(Vector2d (Space @ Unitless)) Self
    ]

direction2d :: Class
direction2d =
  Class.new @(Direction2d Space) $(docs ''Direction2d) $
    [ upcast Vector2d.unit
    , constant "X" Direction2d.x $(docs 'Direction2d.x)
    , constant "Y" Direction2d.y $(docs 'Direction2d.y)
    , factory1 "Polar" "Angle" Direction2d.polar $(docs 'Direction2d.polar)
    , factory1 "Degrees" "Value" Direction2d.degrees $(docs 'Direction2d.degrees)
    , factory1 "Radians" "Value" Direction2d.radians $(docs 'Direction2d.radians)
    , property "Components" (.components) $(docs 'Direction2d.components)
    , property "X Component" (.xComponent) $(docs 'Direction2d.xComponent)
    , property "Y Component" (.yComponent) $(docs 'Direction2d.yComponent)
    , member0 "Rotate Leftward" Direction2d.rotateLeftward $(docs 'Direction2d.rotateLeftward)
    , member0 "Rotate Rightward" Direction2d.rotateRightward $(docs 'Direction2d.rotateRightward)
    , negateSelf
    ]

point2d :: Class
point2d =
  Class.new @(Point2d (Space @ Meters)) "A point in 2D, defined by its X and Y coordinates." $
    [ constant "Origin" (Point2d.origin @Space @Meters) $(docs 'Point2d.origin)
    , constructor2 "X Coordinate" "Y Coordinate" Point2d $(docs 'Point2d)
    , factory1 "X" "X Coordinate" Point2d.x $(docs 'Point2d.x)
    , factory1 "Y" "Y Coordinate" Point2d.y $(docs 'Point2d.y)
    , factory2 "Polar" "Radius" "Angle" Point2d.polar $(docs 'Point2d.polar)
    , factory2 "Meters" "X Coordinate" "Y Coordinate" Point2d.meters $(docs 'Point2d.meters)
    , factory2 "Centimeters" "X Coordinate" "Y Coordinate" Point2d.centimeters $(docs 'Point2d.centimeters)
    , factory2 "Cm" "X Coordinate" "Y Coordinate" Point2d.cm $(docs 'Point2d.cm)
    , factory2 "Millimeters" "X Coordinate" "Y Coordinate" Point2d.millimeters $(docs 'Point2d.millimeters)
    , factory2 "Mm" "X Coordinate" "Y Coordinate" Point2d.mm $(docs 'Point2d.mm)
    , factory2 "Inches" "X Coordinate" "Y Coordinate" Point2d.inches $(docs 'Point2d.inches)
    , factory1 "From Coordinates" "Coordinates" Point2d.fromCoordinates $(docs 'Point2d.fromCoordinates)
    , property "Coordinates" (.coordinates) $(docs 'Point2d.coordinates)
    , property "X Coordinate" (.xCoordinate) $(docs 'Point2d.xCoordinate)
    , property "Y Coordinate" (.yCoordinate) $(docs 'Point2d.yCoordinate)
    , member1 "Distance To" "Other" Point2d.distanceFrom $(docs 'Point2d.distanceFrom)
    , member1 "Midpoint" "Other" Point2d.midpoint $(docs 'Point2d.midpoint)
    , member1 "On" "Plane" Point2d.on $(docs 'Point2d.on)
    , minusSelf
    , minus @(Vector2d (Space @ Meters)) Self
    , plus @(Vector2d (Space @ Meters)) Self
    , minus @(Curve2d (Space @ Meters)) Self
    ]
      <> affineTransformations2d Point2d.transformBy

uvPoint :: Class
uvPoint =
  Class.new @(Point2d (Space @ Unitless)) "A point in UV parameter space." $
    [ constant "Origin" (Point2d.origin @Space @Unitless) "The point with coordinates (0,0)."
    , constructor2 "U Coordinate" "V Coordinate" Point2d "Construct a point from its U and V coordinates."
    , factory1 "From Coordinates" "Coordinates" Point2d.fromCoordinates "Construct a point from a pair of U and V coordinates."
    , property "Coordinates" (.coordinates) "Get the U and V coordinates of a point."
    , property "U Coordinate" (.xCoordinate) "Get the U coordinate of a point."
    , property "V Coordinate" (.yCoordinate) "Get the V coordinate of a point."
    , member1 "Distance To" "Other" Point2d.distanceFrom $(docs 'Point2d.distanceFrom)
    , member1 "Midpoint" "Other" Point2d.midpoint $(docs 'Point2d.midpoint)
    , minusSelf
    , minus @(Vector2d (Space @ Unitless)) Self
    , plus @(Vector2d (Space @ Unitless)) Self
    ]
      <> affineTransformations2d Point2d.transformBy

bounds2d :: Class
bounds2d =
  Class.new @(Bounds2d (Space @ Meters)) "A bounding box in 2D." $
    [ constructor2 "X Coordinate" "Y Coordinate" Bounds2d $(docs 'Bounds2d)
    , factory1 "Constant" "Point" Bounds2d.constant $(docs 'Bounds2d.constant)
    , factory2 "From Corners" "First Point" "Second Point" Bounds2d.hull2 $(docs 'Bounds2d.hull2)
    , factory1 "Hull" "Points" (Bounds2d.hullN @(Point2d (Space @ Meters))) $(docs 'Bounds2d.hullN)
    , factory1 "Aggregate" "Bounds" Bounds2d.aggregateN $(docs 'Bounds2d.aggregateN)
    , property "Coordinates" (.coordinates) $(docs 'Bounds2d.coordinates)
    , property "X Coordinate" (.xCoordinate) $(docs 'Bounds2d.xCoordinate)
    , property "Y Coordinate" (.yCoordinate) $(docs 'Bounds2d.yCoordinate)
    , plus @(Vector2d (Space @ Meters)) Self
    , minus @(Vector2d (Space @ Meters)) Self
    ]
      <> affineTransformations2d Bounds2d.transformBy

uvBounds :: Class
uvBounds =
  Class.new @(Bounds2d (Space @ Unitless)) "A bounding box in UV parameter space." $
    [ constructor2 "U Coordinate" "V Coordinate" Bounds2d "Construct a bounding box from its U and V coordinate bounds."
    , factory1 "Constant" "Point" Bounds2d.constant $(docs 'Bounds2d.constant)
    , factory2 "From Corners" "First Point" "Second Point" Bounds2d.hull2 $(docs 'Bounds2d.hull2)
    , factory1 "Hull" "Points" (Bounds2d.hullN @(Point2d (Space @ Unitless))) $(docs 'Bounds2d.hullN)
    , factory1 "Aggregate" "Bounds" Bounds2d.aggregateN $(docs 'Bounds2d.aggregateN)
    , property "Coordinates" (.coordinates) $(docs 'Bounds2d.coordinates)
    , property "U Coordinate" (.xCoordinate) "Get the U coordinate bounds of a bounding box."
    , property "V Coordinate" (.yCoordinate) "Get the V coordinate bounds of a bounding box."
    , plus @(Vector2d (Space @ Unitless)) Self
    , minus @(Vector2d (Space @ Unitless)) Self
    ]
      <> affineTransformations2d Bounds2d.transformBy

curve :: Class
curve =
  Class.new @(Curve Unitless) "A parametric curve definining a unitless value in terms of a parameter value." $
    [ constant "Zero" (Curve.zero @Unitless) $(docs 'Curve.zero)
    , constant "T" Curve.t $(docs 'Curve.t)
    , factory1 "Constant" "Value" Curve.constant $(docs 'Curve.constant)
    , factory2 "Line" "Start" "End" Curve.line $(docs 'Curve.line)
    , member0 "Squared" Curve.squared $(docs 'Curve.squared)
    , memberU0 "Sqrt" Curve.sqrt $(docs 'Curve.sqrt)
    , member1 "Evaluate" "Parameter Value" (flip Curve.evaluate) $(docs 'Curve.evaluate)
    , memberU0 "Zeros" Curve.zeros $(docs 'Curve.zeros)
    , memberU0 "Is Zero" (~= 0.0) "Check if a curve is zero everywhere, within the current tolerance."
    , negateSelf
    , floatPlus
    , floatMinus
    , floatTimes
    , floatDivBy
    , plusFloat
    , plusSelf
    , minusFloat
    , minusSelf
    , timesFloat
    , timesSelf
    , times @Length Self
    , times @Area Self
    , times @Angle Self
    , times @(Curve Meters) Self
    , times @(Curve SquareMeters) Self
    , times @(Curve Radians) Self
    , divByFloat
    , divBySelf
    , nested @Curve.Zero
        "A point where a given curve is equal to zero."
        [ property "Location" (.location) "The parameter value at which the curve is zero."
        , property "Order" (.order) "The order of the solution: 0 for crossing, 1 for tangent, etc."
        , property "Sign" (.sign) "The sign of the solution: the sign of the curve to the right of the solution."
        ]
    ]

angleCurve :: Class
angleCurve =
  Class.new @(Curve Radians) "A parametric curve definining an angle in terms of a parameter value." $
    [ constant "Zero" (Curve.zero @Radians) $(docs 'Curve.zero)
    , factory1 "Constant" "Value" Curve.constant $(docs 'Curve.constant)
    , factory2 "Line" "Start" "End" Curve.line $(docs 'Curve.line)
    , member0 "Sin" Curve.sin $(docs 'Curve.sin)
    , member0 "Cos" Curve.cos $(docs 'Curve.cos)
    , member1 "Evaluate" "Parameter Value" (flip Curve.evaluate) $(docs 'Curve.evaluate)
    , memberR0 "Zeros" Curve.zeros $(docs 'Curve.zeros)
    , memberR0 "Is Zero" (~= Angle.zero) "Check if a curve is zero everywhere, within the current tolerance."
    , negateSelf
    , floatTimes
    , plusSelf
    , plus @Angle Self
    , minusSelf
    , minus @Angle Self
    , timesFloat
    , times @(Curve Unitless) Self
    , divByFloat
    , divBySelf
    , divBy @Angle Self
    , divBy @(Curve Unitless) Self
    ]

lengthCurve :: Class
lengthCurve =
  Class.new @(Curve Meters) "A parametric curve definining a length in terms of a parameter value." $
    [ constant "Zero" (Curve.zero @Meters) $(docs 'Curve.zero)
    , factory1 "Constant" "Value" Curve.constant $(docs 'Curve.constant)
    , factory2 "Line" "Start" "End" Curve.line $(docs 'Curve.line)
    , member0 "Squared" Curve.squared $(docs 'Curve.squared)
    , member1 "Evaluate" "Parameter Value" (flip Curve.evaluate) $(docs 'Curve.evaluate)
    , memberM0 "Zeros" Curve.zeros $(docs 'Curve.zeros)
    , memberM0 "Is Zero" (~= Length.zero) "Check if a curve is zero everywhere, within the current tolerance."
    , negateSelf
    , floatTimes
    , plusSelf
    , plus @Length Self
    , minusSelf
    , minus @Length Self
    , timesFloat
    , timesSelf
    , times @Length Self
    , times @(Curve Unitless) Self
    , divByFloat
    , divBySelf
    , divBy @Length Self
    , divBy @(Curve Unitless) Self
    ]

areaCurve :: Class
areaCurve =
  Class.new @(Curve SquareMeters) "A parametric curve definining an area in terms of a parameter value." $
    [ constant "Zero" (Curve.zero @SquareMeters) $(docs 'Curve.zero)
    , factory1 "Constant" "Value" Curve.constant $(docs 'Curve.constant)
    , factory2 "Line" "Start" "End" Curve.line $(docs 'Curve.line)
    , memberM0 "Sqrt" Curve.sqrt $(docs 'Curve.sqrt)
    , member1 "Evaluate" "Parameter Value" (flip Curve.evaluate) $(docs 'Curve.evaluate)
    , memberS0 "Zeros" Curve.zeros $(docs 'Curve.zeros)
    , memberS0 "Is Zero" (~= Area.zero) "Check if a curve is zero everywhere, within the current tolerance."
    , negateSelf
    , floatTimes
    , plusSelf
    , plus @Area Self
    , minusSelf
    , minus @Area Self
    , timesFloat
    , times @(Curve Unitless) Self
    , divByFloat
    , divBySelf
    , divBy @Length Self
    , divBy @Area Self
    , divBy @(Curve Unitless) Self
    , divBy @(Curve Meters) Self
    ]

drawing2d :: Class
drawing2d =
  Class.new @(Drawing2d Space) $(docs ''Drawing2d) $
    [ member1 "To SVG" "View Box" Drawing2d.toSvg $(docs 'Drawing2d.toSvg)
    , member2 "Write SVG" "Path" "View Box" Drawing2d.writeSvg $(docs 'Drawing2d.writeSvg)
    , factory1 "Group" "Drawings" Drawing2d.group $(docs 'Drawing2d.group)
    , factory2 "Group With" "Attributes" "Drawings" Drawing2d.groupWith $(docs 'Drawing2d.groupWith)
    , factory1 "Polygon" "Vertices" Drawing2d.polygon $(docs 'Drawing2d.polygon)
    , factory2 "Polygon With" "Attributes" "Vertices" Drawing2d.polygonWith $(docs 'Drawing2d.polygonWith)
    , factory2 "Circle" "Center Point" "Diameter" (curryT2 Drawing2d.circle) $(docs 'Drawing2d.circle)
    , factory3 "Circle With" "Attributes" "Center Point" "Diameter" (curry1T2 Drawing2d.circleWith) $(docs 'Drawing2d.circleWith)
    , factory2 "Curve" "Resolution" "Curve" Drawing2d.curve $(docs 'Drawing2d.curve)
    , factory3 "Curve With" "Attributes" "Resolution" "Curve" Drawing2d.curveWith $(docs 'Drawing2d.curveWith)
    , constant "Black Stroke" Drawing2d.blackStroke $(docs 'Drawing2d.blackStroke)
    , static1 "Stroke Color" "Color" Drawing2d.strokeColor $(docs 'Drawing2d.strokeColor)
    , constant "No Fill" Drawing2d.noFill $(docs 'Drawing2d.noFill)
    , static1 "Fill Color" "Color" Drawing2d.fillColor $(docs 'Drawing2d.fillColor)
    , nested @(Drawing2d.Attribute Space) "A drawing attribute such as fill color or stroke width." []
    ]

axis2d :: Class
axis2d =
  Class.new @(Axis2d (Space @ Meters)) $(docs ''Axis2d) $
    [ constructor2 "Origin Point" "Direction" Axis2d $(docs 'Axis2d)
    , property "Origin Point" (.originPoint) $(docs 'Axis2d.originPoint)
    , property "Direction" (.direction) $(docs 'Axis2d.direction)
    , constant "X" (Axis2d.x @Space @Meters) $(docs 'Axis2d.x)
    , constant "Y" (Axis2d.y @Space @Meters) $(docs 'Axis2d.y)
    ]
      <> orthonormalTransformations2d Axis2d.transformBy

uvAxis :: Class
uvAxis =
  Class.new @(Axis2d (Space @ Unitless)) $(docs ''Axis2d) $
    [ constructor2 "Origin Point" "Direction" Axis2d $(docs 'Axis2d)
    , property "Origin Point" (.originPoint) $(docs 'Axis2d.originPoint)
    , property "Direction" (.direction) $(docs 'Axis2d.direction)
    , constant "U" (Axis2d.x @Space @Meters) "The U axis."
    , constant "V" (Axis2d.y @Space @Meters) "The V axis."
    ]

convention3d :: Class
convention3d =
  Class.new @Convention3d $(docs ''Convention3d) $
    [ constant "Y Up" Convention3d.yUp $(docs 'Convention3d.yUp)
    , constant "Z Up" Convention3d.zUp $(docs 'Convention3d.zUp)
    ]

vector3d :: Class
vector3d =
  Class.new @(Vector3d (Space @ Unitless)) "A unitless vector in 3D." $
    [ constant "Zero" (Vector3d.zero @Space @Unitless) $(docs 'Vector3d.zero)
    , factory1 "Unit" "Direction" Vector3d.unit $(docs 'Vector3d.unit)
    , factory2 "From Components" "Convention" "Components" Vector3d.fromComponents $(docs 'Vector3d.fromComponents)
    , member1 "Components" "Convention" Vector3d.components $(docs 'Vector3d.components)
    , memberU0 "Direction" Vector3d.direction $(docs 'Vector3d.direction)
    , memberU0 "Is Zero" (~= Vector3d.zero) "Check if a vector is zero, within the current tolerance."
    , member2 "Rotate In" "Direction" "Angle" Vector3d.rotateIn $(docs 'Vector3d.rotateIn)
    , member2 "Rotate Around" "Axis" "Angle" (Vector3d.rotateAround @Space @Meters) $(docs 'Vector3d.rotateAround)
    , member1 "Mirror In" "Direction" Vector3d.mirrorIn $(docs 'Vector3d.mirrorIn)
    , member1 "Mirror Across" "Plane" (Vector3d.mirrorAcross @Space @Meters) $(docs 'Vector3d.mirrorAcross)
    , member2 "Scale In" "Direction" "Scale" Vector3d.scaleIn $(docs 'Vector3d.scaleIn)
    , member2 "Scale Along" "Axis" "Scale" (Vector3d.scaleAlong @Space @Meters) $(docs 'Vector3d.scaleAlong)
    , member1 "Place In" "Frame" (Vector3d.placeIn @Space @Meters) $(docs 'Vector3d.placeIn)
    , member1 "Relative To" "Frame" (Vector3d.relativeTo @Space @Meters) $(docs 'Vector3d.relativeTo)
    , negateSelf
    , floatTimes
    , plusSelf
    , minusSelf
    , timesFloat
    , times @Length Self
    , times @Area Self
    , divByFloat
    , dotSelf
    , dotProduct @(Vector3d (Space @ Meters)) Self
    , dotProduct @(Vector3d (Space @ SquareMeters)) Self
    , crossSelf
    , crossProduct @(Vector3d (Space @ Meters)) Self
    , crossProduct @(Vector3d (Space @ SquareMeters)) Self
    ]

displacement3d :: Class
displacement3d =
  Class.new @(Vector3d (Space @ Meters)) "A displacement vector in 3D." $
    [ constant "Zero" (Vector3d.zero @Space @Meters) $(docs 'Vector3d.zero)
    , factory2 "From Components" "Convention" "Components" Vector3d.fromComponents $(docs 'Vector3d.fromComponents)
    , member1 "Components" "Convention" Vector3d.components $(docs 'Vector3d.components)
    , memberM0 "Direction" Vector3d.direction $(docs 'Vector3d.direction)
    , memberM0 "Is Zero" (~= Vector3d.zero) "Check if a displacement is zero, within the current tolerance."
    , member2 "Rotate In" "Direction" "Angle" Vector3d.rotateIn $(docs 'Vector3d.rotateIn)
    , member2 "Rotate Around" "Axis" "Angle" (Vector3d.rotateAround @Space @Meters) $(docs 'Vector3d.rotateAround)
    , member1 "Mirror In" "Direction" Vector3d.mirrorIn $(docs 'Vector3d.mirrorIn)
    , member1 "Mirror Across" "Plane" (Vector3d.mirrorAcross @Space @Meters) $(docs 'Vector3d.mirrorAcross)
    , member2 "Scale In" "Direction" "Scale" Vector3d.scaleIn $(docs 'Vector3d.scaleIn)
    , member2 "Scale Along" "Axis" "Scale" (Vector3d.scaleAlong @Space @Meters) $(docs 'Vector3d.scaleAlong)
    , member1 "Place In" "Frame" (Vector3d.placeIn @Space @Meters) $(docs 'Vector3d.placeIn)
    , member1 "Relative To" "Frame" (Vector3d.relativeTo @Space @Meters) $(docs 'Vector3d.relativeTo)
    , negateSelf
    , floatTimes
    , plusSelf
    , minusSelf
    , timesFloat
    , times @Length Self
    , divByFloat
    , divBy @Length Self
    , dotSelf
    , dotProduct @(Vector3d (Space @ Unitless)) Self
    , crossSelf
    , crossProduct @(Vector3d (Space @ Unitless)) Self
    ]

areaVector3d :: Class
areaVector3d =
  Class.new @(Vector3d (Space @ SquareMeters)) "A vector in 3D with units of area." $
    [ constant "Zero" (Vector3d.zero @Space @SquareMeters) $(docs 'Vector3d.zero)
    , factory2 "From Components" "Convention" "Components" Vector3d.fromComponents $(docs 'Vector3d.fromComponents)
    , member1 "Components" "Convention" Vector3d.components $(docs 'Vector3d.components)
    , memberS0 "Direction" Vector3d.direction $(docs 'Vector3d.direction)
    , memberS0 "Is Zero" (~= Vector3d.zero) "Check if an area vector is zero, within the current tolerance."
    , member2 "Rotate In" "Direction" "Angle" Vector3d.rotateIn $(docs 'Vector3d.rotateIn)
    , member2 "Rotate Around" "Axis" "Angle" (Vector3d.rotateAround @Space @Meters) $(docs 'Vector3d.rotateAround)
    , member1 "Mirror In" "Direction" Vector3d.mirrorIn $(docs 'Vector3d.mirrorIn)
    , member1 "Mirror Across" "Plane" (Vector3d.mirrorAcross @Space @Meters) $(docs 'Vector3d.mirrorAcross)
    , member2 "Scale In" "Direction" "Scale" Vector3d.scaleIn $(docs 'Vector3d.scaleIn)
    , member2 "Scale Along" "Axis" "Scale" (Vector3d.scaleAlong @Space @Meters) $(docs 'Vector3d.scaleAlong)
    , member1 "Place In" "Frame" (Vector3d.placeIn @Space @Meters) $(docs 'Vector3d.placeIn)
    , member1 "Relative To" "Frame" (Vector3d.relativeTo @Space @Meters) $(docs 'Vector3d.relativeTo)
    , negateSelf
    , floatTimes
    , plusSelf
    , minusSelf
    , timesFloat
    , divByFloat
    , divBy @Length Self
    , divBy @Area Self
    , dotProduct @(Vector3d (Space @ Unitless)) Self
    , crossProduct @(Vector3d (Space @ Unitless)) Self
    ]

direction3d :: Class
direction3d =
  Class.new @(Direction3d Space) $(docs ''Direction3d) $
    [ upcast Vector3d.unit
    , member0 "Perpendicular Direction" Direction3d.perpendicularDirection $(docs 'Direction3d.perpendicularDirection)
    , member1 "Angle To" "Other" Direction3d.angleFrom $(docs 'Direction3d.angleFrom)
    , member2 "Rotate In" "Direction" "Angle" Direction3d.rotateIn $(docs 'Direction3d.rotateIn)
    , member2 "Rotate Around" "Axis" "Angle" (Direction3d.rotateAround @Space @Meters) $(docs 'Direction3d.rotateAround)
    , member1 "Mirror In" "Direction" Direction3d.mirrorIn $(docs 'Direction3d.mirrorIn)
    , member1 "Mirror Across" "Plane" (Direction3d.mirrorAcross @Space @Meters) $(docs 'Direction3d.mirrorAcross)
    , member1 "Place In" "Frame" (Direction3d.placeIn @Space @Meters) $(docs 'Direction3d.placeIn)
    , member1 "Relative To" "Frame" (Direction3d.relativeTo @Space @Meters) $(docs 'Direction3d.relativeTo)
    , negateSelf
    ]

point3d :: Class
point3d =
  Class.new @(Point3d (Space @ Meters)) "A point in 3D." $
    [ factory2 "Along" "Axis" "Distance" Point3d.along $(docs 'Point3d.along)
    , factory2 "On" "Plane" "Position" Point3d.on $(docs 'Point3d.on)
    , factory2 "From Coordinates" "Convention" "Coordinates" Point3d.fromCoordinates $(docs 'Point3d.fromCoordinates)
    , member1 "Coordinates" "Convention" Point3d.coordinates $(docs 'Point3d.coordinates)
    , member1 "Distance To" "Other" Point3d.distanceFrom $(docs 'Point3d.distanceFrom)
    , member1 "Midpoint" "Other" Point3d.midpoint $(docs 'Point3d.midpoint)
    , member1 "Project Onto" "Plane" Point3d.projectOnto $(docs 'Point3d.projectOnto)
    , member1 "Project Into" "Plane" Point3d.projectInto $(docs 'Point3d.projectInto)
    , minusSelf
    , minus @(Vector3d (Space @ Meters)) Self
    , plus @(Vector3d (Space @ Meters)) Self
    , member1 "Place In" "Frame" Point3d.placeIn $(docs 'Point3d.placeIn)
    , member1 "Relative To" "Frame" Point3d.relativeTo $(docs 'Point3d.relativeTo)
    ]
      <> affineTransformations3d Point3d.transformBy

bounds3d :: Class
bounds3d =
  Class.new @(Bounds3d (Space @ Meters)) "A bounding box in 3D." $
    [ factory1 "Constant" "Point" Bounds3d.constant $(docs 'Bounds3d.constant)
    , factory2 "From Corners" "First Point" "Second Point" Bounds3d.hull2 $(docs 'Bounds3d.hull2)
    , factory1 "Hull" "Points" (Bounds3d.hullN @(Point3d (Space @ Meters))) $(docs 'Bounds3d.hullN)
    , factory1 "Aggregate" "Bounds" Bounds3d.aggregateN $(docs 'Bounds3d.aggregateN)
    , member1 "Coordinates" "Convention" Bounds3d.coordinates $(docs 'Bounds3d.coordinates)
    , plus @(Vector3d (Space @ Meters)) Self
    , minus @(Vector3d (Space @ Meters)) Self
    ]
      <> affineTransformations3d Bounds3d.transformBy

axis3d :: Class
axis3d =
  Class.new @(Axis3d (Space @ Meters)) $(docs ''Axis3d) $
    [ property "Origin Point" (.originPoint) $(docs 'Axis3d.originPoint)
    , property "Direction" (.direction) $(docs 'Axis3d.direction)
    , member0 "Normal Plane" Axis3d.normalPlane $(docs 'Axis3d.normalPlane)
    , member1 "Move To" "Point" Axis3d.moveTo $(docs 'Axis3d.moveTo)
    , member0 "Reverse" Axis3d.reverse $(docs 'Axis3d.reverse)
    , member1 "Place In" "Frame" Axis3d.placeIn $(docs 'Axis3d.placeIn)
    , member1 "Relative To" "Frame" Axis3d.relativeTo $(docs 'Axis3d.relativeTo)
    ]
      <> orthonormalTransformations3d Axis3d.transformBy

planeOrientation3d :: Class
planeOrientation3d =
  Class.new @(PlaneOrientation3d Space) $(docs ''PlaneOrientation3d) $
    [ factory1 "From Normal Direction" "Direction" PlaneOrientation3d.fromNormalDirection $(docs 'PlaneOrientation3d.fromNormalDirection)
    , factory1 "From X Direction" "Direction" PlaneOrientation3d.fromXDirection $(docs 'PlaneOrientation3d.fromXDirection)
    , factory1 "From Y Direction" "Direction" PlaneOrientation3d.fromYDirection $(docs 'PlaneOrientation3d.fromYDirection)
    , property "X Direction" (.xDirection) $(docs 'PlaneOrientation3d.xDirection)
    , property "Y Direction" (.yDirection) $(docs 'PlaneOrientation3d.yDirection)
    , property "Normal Direction" (.normalDirection) $(docs 'PlaneOrientation3d.normalDirection)
    , member1 "Place In" "Frame" (PlaneOrientation3d.placeIn @Space @Meters) $(docs 'PlaneOrientation3d.placeIn)
    , member1 "Relative To" "Frame" (PlaneOrientation3d.relativeTo @Space @Meters) $(docs 'PlaneOrientation3d.relativeTo)
    ]

plane3d :: Class
plane3d =
  Class.new @(Plane3d (Space @ Meters) (Defines Space)) $(docs ''Plane3d) $
    [ factory2 "From Point And Normal" "Origin Point" "Normal Direction" Plane3d.fromPointAndNormal $(docs 'Plane3d.fromPointAndNormal)
    , factory1 "From X Axis" "Axis" Plane3d.fromXAxis $(docs 'Plane3d.fromXAxis)
    , factory1 "From Y Axis" "Axis" Plane3d.fromYAxis $(docs 'Plane3d.fromYAxis)
    , property "Origin Point" (.originPoint) $(docs 'Plane3d.originPoint)
    , property "X Direction" (.xDirection) $(docs 'Plane3d.xDirection)
    , property "Y Direction" (.yDirection) $(docs 'Plane3d.yDirection)
    , property "Normal Direction" (.normalDirection) $(docs 'Plane3d.normalDirection)
    , property "X Axis" (.xAxis) $(docs 'Plane3d.xAxis)
    , property "Y Axis" (.yAxis) $(docs 'Plane3d.yAxis)
    , property "Normal Axis" (.normalAxis) $(docs 'Plane3d.normalAxis)
    , member1 "Move To" "Point" Plane3d.moveTo $(docs 'Plane3d.moveTo)
    , member0 "Flip" Plane3d.flip $(docs 'Plane3d.flip)
    , member1 "Offset By" "Distance" Plane3d.offsetBy $(docs 'Plane3d.offsetBy)
    , member1 "Place In" "Frame" Plane3d.placeIn $(docs 'Plane3d.placeIn)
    , member1 "Relative To" "Frame" Plane3d.relativeTo $(docs 'Plane3d.relativeTo)
    ]
      <> rigidTransformations3d Plane3d.transformBy

orientation3d :: Class
orientation3d =
  Class.new @(Orientation3d Space) $(docs ''Orientation3d) $
    [ constant "World" (Orientation3d.world @Space) $(docs 'Orientation3d.world)
    , property "Forward Direction" (.forwardDirection) $(docs 'Orientation3d.forwardDirection)
    , property "Backward Direction" (.backwardDirection) $(docs 'Orientation3d.backwardDirection)
    , property "Leftward Direction" (.leftwardDirection) $(docs 'Orientation3d.leftwardDirection)
    , property "Rightward Direction" (.rightwardDirection) $(docs 'Orientation3d.rightwardDirection)
    , property "Upward Direction" (.upwardDirection) $(docs 'Orientation3d.upwardDirection)
    , property "Downward Direction" (.downwardDirection) $(docs 'Orientation3d.downwardDirection)
    , property "Front Plane Orientation" (.frontPlaneOrientation) $(docs 'Orientation3d.frontPlaneOrientation)
    , property "Back Plane Orientation" (.backPlaneOrientation) $(docs 'Orientation3d.backPlaneOrientation)
    , property "Right Plane Orientation" (.rightPlaneOrientation) $(docs 'Orientation3d.rightPlaneOrientation)
    , property "Left Plane Orientation" (.leftPlaneOrientation) $(docs 'Orientation3d.leftPlaneOrientation)
    , property "Bottom Plane Orientation" (.bottomPlaneOrientation) $(docs 'Orientation3d.bottomPlaneOrientation)
    , property "Top Plane Orientation" (.topPlaneOrientation) $(docs 'Orientation3d.topPlaneOrientation)
    , property "Backward Orientation" (.backwardOrientation) $(docs 'Orientation3d.backwardOrientation)
    , property "Rightward Orientation" (.rightwardOrientation) $(docs 'Orientation3d.rightwardOrientation)
    , property "Leftward Orientation" (.leftwardOrientation) $(docs 'Orientation3d.leftwardOrientation)
    , property "Upward Orientation" (.upwardOrientation) $(docs 'Orientation3d.upwardOrientation)
    , property "Downward Orientation" (.downwardOrientation) $(docs 'Orientation3d.downwardOrientation)
    , member1 "Place In" "Frame" (Orientation3d.placeIn @Space @Meters) $(docs 'Orientation3d.placeIn)
    , member1 "Relative To" "Frame" (Orientation3d.relativeTo @Space @Meters) $(docs 'Orientation3d.relativeTo)
    ]

frame3d :: Class
frame3d =
  Class.new @(Frame3d (Space @ Meters) (Defines Space)) $(docs ''Frame3d) $
    [ constant "World" (Frame3d.world @Space @Meters) $(docs 'Frame3d.world)
    , factory1 "Forward" "Reference Frame" Frame3d.forward $(docs 'Frame3d.forward)
    , factory1 "Backward" "Reference Frame" Frame3d.backward $(docs 'Frame3d.backward)
    , factory1 "Rightward" "Reference Frame" Frame3d.rightward $(docs 'Frame3d.rightward)
    , factory1 "Leftward" "Reference Frame" Frame3d.leftward $(docs 'Frame3d.leftward)
    , factory1 "Upward" "Reference Frame" Frame3d.upward $(docs 'Frame3d.upward)
    , factory1 "Downward" "Reference Frame" Frame3d.downward $(docs 'Frame3d.downward)
    , factory1 "From Front Plane" "Plane" Frame3d.fromFrontPlane $(docs 'Frame3d.fromFrontPlane)
    , factory1 "From Back Plane" "Plane" Frame3d.fromBackPlane $(docs 'Frame3d.fromBackPlane)
    , factory1 "From Right Plane" "Plane" Frame3d.fromRightPlane $(docs 'Frame3d.fromRightPlane)
    , factory1 "From Left Plane" "Plane" Frame3d.fromLeftPlane $(docs 'Frame3d.fromLeftPlane)
    , factory1 "From Top Plane" "Plane" Frame3d.fromTopPlane $(docs 'Frame3d.fromTopPlane)
    , factory1 "From Bottom Plane" "Plane" Frame3d.fromBottomPlane $(docs 'Frame3d.fromBottomPlane)
    , factory2 "Align" "Frame" "Reference Frame" Frame3d.align $(docs 'Frame3d.align)
    , factory2 "Mate" "Frame" "Reference Frame" Frame3d.mate $(docs 'Frame3d.mate)
    , property "Origin Point" (.originPoint) $(docs 'Frame3d.originPoint)
    , property "Forward Direction" (.forwardDirection) $(docs 'Frame3d.forwardDirection)
    , property "Backward Direction" (.backwardDirection) $(docs 'Frame3d.backwardDirection)
    , property "Rightward Direction" (.rightwardDirection) $(docs 'Frame3d.rightwardDirection)
    , property "Leftward Direction" (.leftwardDirection) $(docs 'Frame3d.leftwardDirection)
    , property "Upward Direction" (.upwardDirection) $(docs 'Frame3d.upwardDirection)
    , property "Downward Direction" (.downwardDirection) $(docs 'Frame3d.downwardDirection)
    , property "Forward Axis" (.forwardAxis) $(docs 'Frame3d.forwardAxis)
    , property "Backward Axis" (.backwardAxis) $(docs 'Frame3d.backwardAxis)
    , property "Rightward Axis" (.rightwardAxis) $(docs 'Frame3d.rightwardAxis)
    , property "Leftward Axis" (.leftwardAxis) $(docs 'Frame3d.leftwardAxis)
    , property "Upward Axis" (.upwardAxis) $(docs 'Frame3d.upwardAxis)
    , property "Downward Axis" (.downwardAxis) $(docs 'Frame3d.downwardAxis)
    , property "Front Plane" (.frontPlane) $(docs 'Frame3d.frontPlane)
    , property "Back Plane" (.backPlane) $(docs 'Frame3d.backPlane)
    , property "Right Plane" (.rightPlane) $(docs 'Frame3d.rightPlane)
    , property "Left Plane" (.leftPlane) $(docs 'Frame3d.leftPlane)
    , property "Top Plane" (.topPlane) $(docs 'Frame3d.topPlane)
    , property "Bottom Plane" (.bottomPlane) $(docs 'Frame3d.bottomPlane)
    , property "Backward Frame" Frame3d.backward $(docs 'Frame3d.backward)
    , property "Leftward Frame" Frame3d.leftward $(docs 'Frame3d.leftward)
    , property "Rightward Frame" Frame3d.rightward $(docs 'Frame3d.rightward)
    , property "Upward Frame" Frame3d.upward $(docs 'Frame3d.upward)
    , property "Downward Frame" Frame3d.downward $(docs 'Frame3d.downward)
    , member1 "Place In" "Other Frame" Frame3d.placeIn $(docs 'Frame3d.placeIn)
    , member1 "Relative To" "Other Frame" Frame3d.relativeTo $(docs 'Frame3d.relativeTo)
    , member0 "Inverse" Frame3d.inverse $(docs 'Frame3d.inverse)
    , member1 "Move To" "Point" Frame3d.moveTo $(docs 'Frame3d.moveTo)
    , member1 "Offset Forward By" "Distance" Frame3d.offsetForwardBy $(docs 'Frame3d.offsetForwardBy)
    , member1 "Offset Backward By" "Distance" Frame3d.offsetBackwardBy $(docs 'Frame3d.offsetBackwardBy)
    , member1 "Offset Rightward By" "Distance" Frame3d.offsetRightwardBy $(docs 'Frame3d.offsetRightwardBy)
    , member1 "Offset Leftward By" "Distance" Frame3d.offsetLeftwardBy $(docs 'Frame3d.offsetLeftwardBy)
    , member1 "Offset Upward By" "Distance" Frame3d.offsetUpwardBy $(docs 'Frame3d.offsetUpwardBy)
    , member1 "Offset Downward By" "Distance" Frame3d.offsetDownwardBy $(docs 'Frame3d.offsetDownwardBy)
    , member1 "Turn Rightward By" "Angle" Frame3d.turnRightwardBy $(docs 'Frame3d.turnRightwardBy)
    , member1 "Turn Leftward By" "Angle" Frame3d.turnLeftwardBy $(docs 'Frame3d.turnLeftwardBy)
    , member1 "Roll Rightward By" "Angle" Frame3d.rollRightwardBy $(docs 'Frame3d.rollRightwardBy)
    , member1 "Roll Leftward By" "Angle" Frame3d.rollLeftwardBy $(docs 'Frame3d.rollLeftwardBy)
    , member1 "Tilt Upward By" "Angle" Frame3d.tiltUpwardBy $(docs 'Frame3d.tiltUpwardBy)
    , member1 "Tilt Downward By" "Angle" Frame3d.tiltDownwardBy $(docs 'Frame3d.tiltDownwardBy)
    , member0 "Turn Rightward" Frame3d.turnRightward $(docs 'Frame3d.turnRightward)
    , member0 "Turn Leftward" Frame3d.turnLeftward $(docs 'Frame3d.turnLeftward)
    , member0 "Roll Rightward" Frame3d.rollRightward $(docs 'Frame3d.rollRightward)
    , member0 "Roll Leftward" Frame3d.rollLeftward $(docs 'Frame3d.rollLeftward)
    , member0 "Tilt Upward" Frame3d.tiltUpward $(docs 'Frame3d.tiltUpward)
    , member0 "Tilt Downward" Frame3d.tiltDownward $(docs 'Frame3d.tiltDownward)
    ]
      <> rigidTransformations3d Frame3d.transformBy

vectorCurve2d :: Class
vectorCurve2d =
  Class.new @(VectorCurve2d (Space @ Unitless)) "A parametric curve defining a 2D unitless vector in terms of a parameter value." $
    [ constant "Zero" (VectorCurve2d.zero @Space @Unitless) $(docs 'VectorCurve2d.zero)
    , factory1 "Constant" "Value" VectorCurve2d.constant $(docs 'VectorCurve2d.constant)
    , factory2 "XY" "X Component" "Y Component" VectorCurve2d.xy $(docs 'VectorCurve2d.xy)
    , member1 "Evaluate" "Parameter Value" (flip VectorCurve2d.evaluate) $(docs 'VectorCurve2d.evaluate)
    ]

displacementCurve2d :: Class
displacementCurve2d =
  Class.new @(VectorCurve2d (Space @ Meters)) "A parametric curve defining a 2D displacement vector in terms of a parameter value." $
    [ constant "Zero" (VectorCurve2d.zero @Space @Meters) $(docs 'VectorCurve2d.zero)
    , factory1 "Constant" "Value" VectorCurve2d.constant $(docs 'VectorCurve2d.constant)
    , factory2 "XY" "X Component" "Y Component" VectorCurve2d.xy $(docs 'VectorCurve2d.xy)
    , member1 "Evaluate" "Parameter Value" (flip VectorCurve2d.evaluate) $(docs 'VectorCurve2d.evaluate)
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
  [ member1 "Translate By" "Displacement" (Transform2d.translateByImpl transformBy) "Translate by the given displacement."
  , member2 "Translate In" "Direction" "Distance" (Transform2d.translateInImpl transformBy) "Translate in the given direction by the given distance."
  , member2 "Translate Along" "Axis" "Distance" (Transform2d.translateAlongImpl transformBy) "Translate along the given axis by the given distance."
  , member2 "Rotate Around" "Point" "Angle" (Transform2d.rotateAroundImpl transformBy) "Rotate around the given point by the given angle."
  ]

orthonormalTransformations2d ::
  Transformation2d value space units =>
  (forall tag. Transform.IsOrthonormal tag => Transform2d tag (space @ units) -> value -> value) ->
  List (Class.Member value)
orthonormalTransformations2d transformBy =
  member1 "Mirror Across" "Axis" (Transform2d.mirrorAcrossImpl transformBy) "Mirror across the given axis."
    : rigidTransformations2d transformBy

uniformTransformations2d ::
  Transformation2d value space units =>
  (forall tag. Transform.IsUniform tag => Transform2d tag (space @ units) -> value -> value) ->
  List (Class.Member value)
uniformTransformations2d transformBy =
  member2 "Scale About" "Point" "Scale" (Transform2d.scaleAboutImpl transformBy) "Scale uniformly about the given point by the given scaling factor."
    : orthonormalTransformations2d transformBy

affineTransformations2d ::
  Transformation2d value space units =>
  (forall tag. Transform2d tag (space @ units) -> value -> value) ->
  List (Class.Member value)
affineTransformations2d transformBy =
  member2 "Scale Along" "Axis" "Scale" (Transform2d.scaleAlongImpl transformBy) "Scale (stretch) along the given axis by the given scaling factor."
    : uniformTransformations2d transformBy

rigidTransformations3d ::
  FFI value =>
  (Transform3d.Rigid (Space @ Meters) -> value -> value) ->
  List (Class.Member value)
rigidTransformations3d transformBy =
  [ member1 "Translate By" "Displacement" (Transform3d.translateByImpl transformBy) "Translate by the given displacement."
  , member2 "Translate In" "Direction" "Distance" (Transform3d.translateInImpl transformBy) "Translate in the given direction by the given distance."
  , member2 "Translate Along" "Axis" "Distance" (Transform3d.translateAlongImpl transformBy) "Translate along the given axis by the given distance."
  , member2 "Rotate Around" "Axis" "Angle" (Transform3d.rotateAroundImpl transformBy) "Rotate around the given axis by the given angle."
  ]

orthonormalTransformations3d ::
  FFI value =>
  (forall tag. Transform.IsOrthonormal tag => Transform3d tag (Space @ Meters) -> value -> value) ->
  List (Class.Member value)
orthonormalTransformations3d transformBy =
  member1 "Mirror Across" "Plane" (Transform3d.mirrorAcrossImpl transformBy) "Mirror across the given plane."
    : rigidTransformations3d transformBy

uniformTransformations3d ::
  FFI value =>
  (forall tag. Transform.IsUniform tag => Transform3d tag (Space @ Meters) -> value -> value) ->
  List (Class.Member value)
uniformTransformations3d transformBy =
  member2 "Scale About" "Point" "Scale" (Transform3d.scaleAboutImpl transformBy) "Scale uniformly about the given point by the given scaling factor."
    : orthonormalTransformations3d transformBy

affineTransformations3d ::
  FFI value =>
  (forall tag. Transform3d tag (Space @ Meters) -> value -> value) ->
  List (Class.Member value)
affineTransformations3d transformBy =
  member2 "Scale Along" "Axis" "Scale" (Transform3d.scaleAlongImpl transformBy) "Scale (stretch) along the given axis by the given scaling factor."
    : uniformTransformations3d transformBy

curve2d :: Class
curve2d =
  Class.new @(Curve2d (Space @ Meters)) $(docs ''Curve2d) $
    [ factory1 "Constant" "Point" Curve2d.constant $(docs 'Curve2d.constant)
    , factory2 "XY" "X Coordinate" "Y Coordinate" Curve2d.xy $(docs 'Curve2d.xy)
    , factory2 "Line" "Start Point" "End Point" Curve2d.line $(docs 'Curve2d.line)
    , factoryM3 "Arc" "Start Point" "End Point" "Swept Angle" Curve2d.arc $(docs 'Curve2d.arc)
    , factory4 "Polar Arc" "Center Point" "Radius" "Start Angle" "End Angle" (curryT4 Curve2d.polarArc) $(docs 'Curve2d.polarArc)
    , factory3 "Swept Arc" "Center Point" "Start Point" "Swept Angle" Curve2d.sweptArc $(docs 'Curve2d.sweptArc)
    , factoryM4 "Corner Arc" "Corner Point" "Incoming" "Outgoing" "Radius" (curry1T3 Curve2d.cornerArc) $(docs 'Curve2d.cornerArc)
    , factory2 "Circle" "Center Point" "Diameter" (curryT2 Curve2d.circle) $(docs 'Curve2d.circle)
    , factory1 "Bezier" "Control Points" Curve2d.bezier $(docs 'Curve2d.bezier)
    , factory4 "Hermite" "Start Point" "Start Derivatives" "End Point" "End Derivatives" Curve2d.hermite $(docs 'Curve2d.hermite)
    , property "Start Point" (.startPoint) "The start point of the curve."
    , property "End Point" (.endPoint) "The end point of the curve."
    , member1 "Evaluate" "Parameter Value" (flip Curve2d.evaluate) $(docs 'Curve2d.evaluate)
    , property "Derivative" (.derivative) "The derivative of the curve."
    , member0 "Reverse" Curve2d.reverse $(docs 'Curve2d.reverse)
    , property "X Coordinate" (.xCoordinate) $(docs 'Curve2d.xCoordinate)
    , property "Y Coordinate" (.yCoordinate) $(docs 'Curve2d.yCoordinate)
    , plus @(VectorCurve2d (Space @ Meters)) Self
    , minus @(VectorCurve2d (Space @ Meters)) Self
    , minusSelf
    , minus @(Point2d (Space @ Meters)) Self
    ]
      <> affineTransformations2d Curve2d.transformBy

uvCurve :: Class
uvCurve =
  Class.new @(Curve2d (Space @ Unitless)) $(docs ''Curve2d) $
    [ factory1 "Constant" "Point" Curve2d.constant $(docs 'Curve2d.constant)
    , factory2 "UV" "U Coordinate" "V Coordinate" Curve2d.xy $(docs 'Curve2d.xy)
    , factory2 "Line" "Start Point" "End Point" Curve2d.line $(docs 'Curve2d.line)
    , factoryU3 "Arc" "Start Point" "End Point" "Swept Angle" Curve2d.arc $(docs 'Curve2d.arc)
    , factory4 "Polar Arc" "Center Point" "Radius" "Start Angle" "End Angle" (curryT4 Curve2d.polarArc) $(docs 'Curve2d.polarArc)
    , factory2 "Circle" "Center Point" "Diameter" (curryT2 Curve2d.circle) $(docs 'Curve2d.circle)
    , factory3 "Swept Arc" "Center Point" "Start Point" "Swept Angle" Curve2d.sweptArc $(docs 'Curve2d.sweptArc)
    , factoryU4 "Corner Arc" "Corner Point" "Incoming" "Outgoing" "Radius" (curry1T3 Curve2d.cornerArc) $(docs 'Curve2d.cornerArc)
    , factory1 "Bezier" "Control Points" Curve2d.bezier $(docs 'Curve2d.bezier)
    , factory4 "Hermite" "Start Point" "Start Derivatives" "End Point" "End Derivatives" Curve2d.hermite $(docs 'Curve2d.hermite)
    , property "Start Point" (.startPoint) "The start point of the curve."
    , property "End Point" (.endPoint) "The end point of the curve."
    , member1 "Evaluate" "Parameter Value" (flip Curve2d.evaluate) $(docs 'Curve2d.evaluate)
    , property "Derivative" (.derivative) "The derivative of the curve."
    , member0 "Reverse" Curve2d.reverse $(docs 'Curve2d.reverse)
    , property "U Coordinate" (.xCoordinate) "Get the U coordinate of a UV curve as a scalar curve."
    , property "V Coordinate" (.yCoordinate) "Get the V coordinate of a UV curve as a scalar curve."
    , plus @(VectorCurve2d (Space @ Unitless)) Self
    , minus @(VectorCurve2d (Space @ Unitless)) Self
    , minusSelf
    , minus @(Point2d (Space @ Unitless)) Self
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
    [ factoryM1R "Bounded By" "Curves" Region2d.boundedBy $(docs 'Region2d.boundedBy)
    , factoryM1R "Rectangle" "Bounding Box" Region2d.rectangle $(docs 'Region2d.rectangle)
    , factoryM2R "Circle" "Center Point" "Diameter" (curryT2 Region2d.circle) $(docs 'Region2d.circle)
    , property "Outer Loop" (.outerLoop) region2dOuterLoopDocs
    , property "Inner Loops" (.innerLoops) region2dInnerLoopsDocs
    , property "Boundary Curves" (.boundaryCurves) region2dBoundaryCurvesDocs
    , memberM2 "Fillet" "Points" "Radius" Region2d.fillet $(docs 'Region2d.fillet)
    ]
      <> affineTransformations2d Region2d.transformBy

uvRegion :: Class
uvRegion =
  Class.new @(Region2d (Space @ Unitless)) $(docs ''Region2d) $
    [ constant "Unit Square" Region2d.unitSquare $(docs 'Region2d.unitSquare)
    , factoryU1R "Bounded By" "Curves" Region2d.boundedBy $(docs 'Region2d.boundedBy)
    , factoryU1R "Rectangle" "Bounding Box" Region2d.rectangle $(docs 'Region2d.rectangle)
    , factoryU2R "Circle" "Center Point" "Diameter" (curryT2 Region2d.circle) $(docs 'Region2d.circle)
    , property "Outer Loop" (.outerLoop) region2dOuterLoopDocs
    , property "Inner Loops" (.innerLoops) region2dInnerLoopsDocs
    , property "Boundary Curves" (.boundaryCurves) region2dBoundaryCurvesDocs
    ]
      <> affineTransformations2d Region2d.transformBy

polygon2d :: Class
polygon2d =
  Class.new @(Polygon2d (Point2d (Space @ Meters))) $(docs ''Polygon2d) $
    [ upcast Region2d.polygon
    , factoryM1R "From Vertices" "Vertices" Polygon2d.fromVertices $(docs 'Polygon2d.fromVertices)
    , factoryM3R "Inscribed" "Num Sides" "Center Point" "Diameter" (curry1T2 Polygon2d.inscribed) $(docs 'Polygon2d.inscribed)
    , factoryM3R "Circumscribed" "Num Sides" "Center Point" "Diameter" (curry1T2 Polygon2d.circumscribed) $(docs 'Polygon2d.circumscribed)
    , factoryM2R "Hexagon" "Center Point" "Height" (curryT2 Polygon2d.hexagon) $(docs 'Polygon2d.hexagon)
    ]

body3d :: Class
body3d = do
  let writeStl path convention givenResolution body =
        Stl.writeBinary path convention Length.inMillimeters (Body3d.toMesh givenResolution body)
  let writeMitsuba path givenResolution body =
        Mitsuba.writeBinary path (Body3d.toMesh givenResolution body)
  Class.new @(Body3d (Space @ Meters)) $(docs ''Body3d) $
    [ factoryM3R "Extruded" "Sketch Plane" "Profile" "Distance" Body3d.extruded $(docs 'Body3d.extruded)
    , factoryM4R "Revolved" "Sketch Plane" "Profile" "Axis" "Angle" Body3d.revolved $(docs 'Body3d.revolved)
    , factoryM1R "Block" "Bounding Box" Body3d.block $(docs 'Body3d.block)
    , factoryM2R "Sphere" "Center Point" "Diameter" (curryT2 Body3d.sphere) $(docs 'Body3d.sphere)
    , factoryM3R "Cylinder" "Start Point" "End Point" "Diameter" Body3d.cylinder $(docs 'Body3d.cylinder)
    , factoryM3R "Cylinder Along" "Axis" "Distance" "Diameter" Body3d.cylinderAlong $(docs 'Body3d.cylinderAlong)
    , member1 "Place In" "Frame" Body3d.placeIn $(docs 'Body3d.placeIn)
    , member1 "Relative To" "Frame" Body3d.relativeTo $(docs 'Body3d.relativeTo)
    , memberM3 "Write STL" "Path" "Convention" "Resolution" writeStl "Write a body to a binary STL file, using units of millimeters."
    , memberM2 "Write Mitsuba" "Path" "Resolution" writeMitsuba "Write a body to Mitsuba 'serialized' file."
    ]

resolution :: Class
resolution =
  Class.new @(Resolution Meters) $(docs ''Resolution) $
    [ factory1 "Max Error" "Error" (Resolution.maxError @Meters) $(docs 'Resolution.maxError)
    , factory1 "Max Size" "Size" (Resolution.maxSize @Meters) $(docs 'Resolution.maxSize)
    , constructor2 "Max Error" "Max Size" (curryT2 Resolution) $(docs 'Resolution)
    ]

pbrMaterial :: Class
pbrMaterial =
  Class.new @PbrMaterial $(docs ''PbrMaterial) $
    [ factory2 "Metal" "Base Color" "Roughness" PbrMaterial.metal $(docs 'PbrMaterial.metal)
    , factory1 "Aluminum" "Roughness" PbrMaterial.aluminum $(docs 'PbrMaterial.aluminum)
    , factory1 "Brass" "Roughness" PbrMaterial.brass $(docs 'PbrMaterial.brass)
    , factory1 "Chromium" "Roughness" PbrMaterial.chromium $(docs 'PbrMaterial.chromium)
    , factory1 "Copper" "Roughness" PbrMaterial.copper $(docs 'PbrMaterial.copper)
    , factory1 "Gold" "Roughness" PbrMaterial.gold $(docs 'PbrMaterial.gold)
    , factory1 "Iron" "Roughness" PbrMaterial.iron $(docs 'PbrMaterial.iron)
    , factory1 "Nickel" "Roughness" PbrMaterial.nickel $(docs 'PbrMaterial.nickel)
    , factory1 "Silver" "Roughness" PbrMaterial.silver $(docs 'PbrMaterial.silver)
    , factory1 "Titanium" "Roughness" PbrMaterial.titanium $(docs 'PbrMaterial.titanium)
    , factory2 "Nonmetal" "Base Color" "Roughness" PbrMaterial.nonmetal $(docs 'PbrMaterial.nonmetal)
    , factory3 "Custom" "Base Color" "Metallic" "Roughness" (curry1T2 PbrMaterial.custom) $(docs 'PbrMaterial.custom)
    ]

scene3d :: Class
scene3d =
  Class.new @(Scene3d Space) $(docs ''Scene3d) $
    [ constant "Nothing" (Scene3d.nothing @Space) $(docs 'Scene3d.nothing)
    , factoryM3 "Body" "Resolution" "Material" "Body" (Scene3d.body @Space) $(docs 'Scene3d.body)
    , factory1 "Group" "Entities" (Scene3d.group @Space) $(docs 'Scene3d.group)
    , member1 "Write GLB" "Path" (Scene3d.writeGlb @Space) $(docs 'Scene3d.writeGlb)
    ]
      <> rigidTransformations3d Scene3d.transformBy

spurGear :: Class
spurGear =
  Class.new @SpurGear $(docs ''SpurGear) $
    [ factory2 "Metric" "Num Teeth" "Module" (curryT2 SpurGear.metric) $(docs 'SpurGear.metric)
    , property "Num Teeth" (.numTeeth) "The number of teeth of a gear."
    , property "Module" (.module_) "The module of a gear."
    , property "Pitch Diameter" (.pitchDiameter) "The pitch diameter of a gear."
    , property "Outer Diameter" (.outerDiameter) "The outer diameter of a gear."
    , memberM0 "Profile" SpurGear.profile $(docs 'SpurGear.profile)
    ]
