module API (classes, functions) where

import API.Class
  ( Class
  , Self (Self)
  , absSelf
  , comparison
  , constant
  , cross
  , crossSelf
  , divBy
  , divByFloat
  , divBySelf
  , dot
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
  , memberR0
  , memberSM0
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
  , static1
  , static2
  , static3
  , staticM3
  , times
  , timesFloat
  , timesSelf
  )
import API.Class qualified as Class
import API.Docs (docs)
import API.Function (Function)
import OpenSolid.Angle (Angle)
import OpenSolid.Angle qualified as Angle
import OpenSolid.Area (Area)
import OpenSolid.Area qualified as Area
import OpenSolid.Axis2d (Axis2d)
import OpenSolid.Axis2d qualified as Axis2d
import OpenSolid.Axis3d (Axis3d)
import OpenSolid.Axis3d qualified as Axis3d
import OpenSolid.Body3d (Body3d)
import OpenSolid.Body3d qualified as Body3d
import OpenSolid.Bounds2d (Bounds2d)
import OpenSolid.Bounds2d qualified as Bounds2d
import OpenSolid.Bounds3d (Bounds3d)
import OpenSolid.Bounds3d qualified as Bounds3d
import OpenSolid.Color (Color)
import OpenSolid.Color qualified as Color
import OpenSolid.Curve (Curve)
import OpenSolid.Curve qualified as Curve
import OpenSolid.Curve.Zero qualified as Curve.Zero
import OpenSolid.Curve2d (Curve2d)
import OpenSolid.Curve2d qualified as Curve2d
import OpenSolid.Direction2d (Direction2d)
import OpenSolid.Direction2d qualified as Direction2d
import OpenSolid.Direction3d (Direction3d)
import OpenSolid.Direction3d qualified as Direction3d
import OpenSolid.Drawing2d qualified as Drawing2d
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Length (Length)
import OpenSolid.Length qualified as Length
import OpenSolid.List qualified as List
import OpenSolid.Mesh qualified as Mesh
import OpenSolid.Plane3d (Plane3d)
import OpenSolid.Plane3d qualified as Plane3d
import OpenSolid.Point2d (Point2d)
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Point3d (Point3d)
import OpenSolid.Point3d qualified as Point3d
import OpenSolid.Prelude
import OpenSolid.Qty qualified as Qty
import OpenSolid.Range (Range)
import OpenSolid.Range qualified as Range
import OpenSolid.Region2d (Region2d)
import OpenSolid.Region2d qualified as Region2d
import OpenSolid.Scene3d qualified as Scene3d
import OpenSolid.Transform qualified as Transform
import OpenSolid.Transform2d (Transform2d)
import OpenSolid.Transform2d qualified as Transform2d
import OpenSolid.Transform3d (Transform3d)
import OpenSolid.Transform3d qualified as Transform3d
import OpenSolid.Units (Meters, Radians, SquareMeters)
import OpenSolid.Vector2d (Vector2d)
import OpenSolid.Vector2d qualified as Vector2d
import OpenSolid.Vector3d (Vector3d)
import OpenSolid.Vector3d qualified as Vector3d
import OpenSolid.VectorCurve2d (VectorCurve2d)
import OpenSolid.VectorCurve2d qualified as VectorCurve2d
import Prelude (flip)

data Space

----- API DEFINITION -----

classes :: List Class
classes =
  [ length
  , area
  , angle
  , range
  , lengthRange
  , areaRange
  , angleRange
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
  , vector3d
  , displacement3d
  , areaVector3d
  , direction3d
  , point3d
  , bounds3d
  , axis3d
  , plane3d
  , vectorCurve2d
  , displacementCurve2d
  , curve2d
  , uvCurve
  , region2d
  , uvRegion
  , body3d
  , mesh
  , scene3d
  ]

length :: Class
length =
  Class.new @Length $(docs ''Length) $
    [ constant "Zero" Length.zero $(docs 'Length.zero)
    , constant "Meter" Length.meter $(docs 'Length.meter)
    , constant "Centimeter" Length.centimeter $(docs 'Length.centimeter)
    , constant "Millimeter" Length.millimeter $(docs 'Length.millimeter)
    , constant "Micrometer" Length.micrometer $(docs 'Length.micrometer)
    , constant "Nanometer" Length.nanometer $(docs 'Length.nanometer)
    , constant "Inch" Length.inch $(docs 'Length.inch)
    , constant "Pixel" Length.pixel $(docs 'Length.pixel)
    , factory1 "Meters" "Value" Length.meters $(docs 'Length.meters)
    , factory1 "Centimeters" "Value" Length.centimeters $(docs 'Length.centimeters)
    , factory1 "Millimeters" "Value" Length.millimeters $(docs 'Length.millimeters)
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
    , plus @(Range Meters) Self
    , plus @(Curve Meters) Self
    , minusSelf
    , minus @(Range Meters) Self
    , minus @(Curve Meters) Self
    , timesFloat
    , timesSelf
    , times @(Range Unitless) Self
    , times @(Range Meters) Self
    , times @(Curve Unitless) Self
    , times @(Curve Meters) Self
    , times @(Direction2d Space) Self
    , times @(Vector2d (Space @ Unitless)) Self
    , times @(Vector2d (Space @ Meters)) Self
    , divByFloat
    , divBySelf
    , divBy @(Range Unitless) Self
    , divBy @(Range Meters) Self
    , divBy @(Curve Unitless) Self
    , divBy @(Curve Meters) Self
    , floorDivBySelf
    , modBySelf
    ]

area :: Class
area =
  Class.new @Area $(docs ''Area) $
    [ constant "Zero" Area.zero $(docs 'Area.zero)
    , constant "Square Meter" Area.squareMeter $(docs 'Area.squareMeter)
    , constant "Square Inch" Area.squareInch $(docs 'Area.squareInch)
    , factory1 "Square Meters" "Value" Area.squareMeters $(docs 'Area.squareMeters)
    , factory1 "Square Inches" "Value" Area.squareInches $(docs 'Area.squareInches)
    , member0 "In Square Meters" Area.inSquareMeters $(docs 'Area.inSquareMeters)
    , member0 "In Square Inches" Area.inSquareInches $(docs 'Area.inSquareInches)
    , memberSM0 "Is Zero" (~= Area.zero) "Check if an area is zero, within the current tolerance."
    , equality
    , comparison
    , negateSelf
    , absSelf Qty.abs
    , floatTimes
    , plusSelf
    , plus @(Range SquareMeters) Self
    , plus @(Curve SquareMeters) Self
    , minusSelf
    , minus @(Range SquareMeters) Self
    , minus @(Curve SquareMeters) Self
    , timesFloat
    , times @(Range Unitless) Self
    , times @(Curve Unitless) Self
    , times @(Direction2d Space) Self
    , times @(Vector2d (Space @ Unitless)) Self
    , divByFloat
    , divBySelf
    , divBy @Length Self
    , divBy @(Range Unitless) Self
    , divBy @(Range Meters) Self
    , divBy @(Range SquareMeters) Self
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
    , constant "Golden Angle" Angle.goldenAngle $(docs 'Angle.goldenAngle)
    , constant "Radian" Angle.radian $(docs 'Angle.radian)
    , constant "Degree" Angle.degree $(docs 'Angle.degree)
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
    , plus @(Range Radians) Self
    , plus @(Curve Radians) Self
    , minusSelf
    , minus @(Range Radians) Self
    , minus @(Curve Radians) Self
    , timesFloat
    , times @(Range Unitless) Self
    , times @(Curve Unitless) Self
    , divByFloat
    , divBySelf
    , divBy @(Range Unitless) Self
    , divBy @(Range Radians) Self
    , divBy @(Curve Unitless) Self
    , divBy @(Curve Radians) Self
    , floorDivBySelf
    , modBySelf
    ]

range :: Class
range =
  Class.new @(Range Unitless) "A range of unitless values, with a lower bound and upper bound." $
    [ constant "Unit" Range.unit $(docs 'Range.unit)
    , factory1 "Constant" "Value" Range.constant $(docs 'Range.constant)
    , factory2 "From Endpoints" "A" "B" Range.from $(docs 'Range.from)
    , factory1 "Hull" "Values" Range.hullN $(docs 'Range.hullN)
    , factory1 "Aggregate" "Ranges" Range.aggregateN $(docs 'Range.aggregateN)
    , member0 "Endpoints" Range.endpoints $(docs 'Range.endpoints)
    , member0 "Lower Bound" Range.lowerBound $(docs 'Range.lowerBound)
    , member0 "Upper Bound" Range.upperBound $(docs 'Range.upperBound)
    , member1 "Intersection" "Other" Range.intersection $(docs 'Range.intersection)
    , member1 "Includes" "Value" Range.includes $(docs 'Range.includes)
    , member1 "Contains" "Other" Range.contains $(docs 'Range.contains)
    , negateSelf
    , absSelf Range.abs
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
    , times @(Range Meters) Self
    , times @(Range SquareMeters) Self
    , times @(Range Radians) Self
    , divByFloat
    , divBySelf
    ]

lengthRange :: Class
lengthRange =
  Class.new @(Range Meters) "A range of length values, with a lower bound and upper bound." $
    [ factory1 "Constant" "Value" Range.constant $(docs 'Range.constant)
    , factory2 "From Endpoints" "A" "B" Range.from $(docs 'Range.from)
    , factory2 "Meters" "A" "B" Range.meters $(docs 'Range.meters)
    , factory2 "Centimeters" "A" "B" Range.centimeters $(docs 'Range.centimeters)
    , factory2 "Millimeters" "A" "B" Range.millimeters $(docs 'Range.millimeters)
    , factory2 "Inches" "A" "B" Range.inches $(docs 'Range.inches)
    , factory1 "Hull" "Values" Range.hullN $(docs 'Range.hullN)
    , factory1 "Aggregate" "Ranges" Range.aggregateN $(docs 'Range.aggregateN)
    , member0 "Endpoints" Range.endpoints $(docs 'Range.endpoints)
    , member1 "Intersection" "Other" Range.intersection $(docs 'Range.intersection)
    , member1 "Includes" "Value" Range.includes $(docs 'Range.includes)
    , member1 "Contains" "Other" Range.contains $(docs 'Range.contains)
    , negateSelf
    , absSelf Range.abs
    , floatTimes
    , plusSelf
    , plus @Length Self
    , minusSelf
    , minus @Length Self
    , timesFloat
    , timesSelf
    , times @Length Self
    , times @(Range Unitless) Self
    , divByFloat
    , divBySelf
    , divBy @Length Self
    , divBy @(Range Unitless) Self
    ]

areaRange :: Class
areaRange =
  Class.new @(Range SquareMeters) "A range of area values, with a lower bound and upper bound." $
    [ factory1 "Constant" "Value" Range.constant $(docs 'Range.constant)
    , factory2 "Square Meters" "A" "B" Range.squareMeters $(docs 'Range.squareMeters)
    , factory2 "From Endpoints" "A" "B" Range.from $(docs 'Range.from)
    , factory1 "Hull" "Values" Range.hullN $(docs 'Range.hullN)
    , factory1 "Aggregate" "Ranges" Range.aggregateN $(docs 'Range.aggregateN)
    , member0 "Endpoints" Range.endpoints $(docs 'Range.endpoints)
    , member1 "Intersection" "Other" Range.intersection $(docs 'Range.intersection)
    , member1 "Includes" "Value" Range.includes $(docs 'Range.includes)
    , member1 "Contains" "Other" Range.contains $(docs 'Range.contains)
    , negateSelf
    , absSelf Range.abs
    , floatTimes
    , plusSelf
    , plus @Area Self
    , minusSelf
    , minus @Area Self
    , timesFloat
    , times @(Range Unitless) Self
    , divByFloat
    , divBySelf
    , divBy @Length Self
    , divBy @Area Self
    , divBy @(Range Unitless) Self
    , divBy @(Range Meters) Self
    ]

angleRange :: Class
angleRange =
  Class.new @(Range Radians) "A range of angle values, with a lower bound and upper bound." $
    [ factory1 "Constant" "Value" Range.constant $(docs 'Range.constant)
    , factory2 "From Endpoints" "A" "B" Range.from $(docs 'Range.from)
    , factory2 "Radians" "A" "B" Range.radians $(docs 'Range.radians)
    , factory2 "Degrees" "A" "B" Range.degrees $(docs 'Range.degrees)
    , factory2 "Turns" "A" "B" Range.turns $(docs 'Range.turns)
    , factory1 "Hull" "Values" Range.hullN $(docs 'Range.hullN)
    , factory1 "Aggregate" "Ranges" Range.aggregateN $(docs 'Range.aggregateN)
    , member0 "Endpoints" Range.endpoints $(docs 'Range.endpoints)
    , member1 "Intersection" "Other" Range.intersection $(docs 'Range.intersection)
    , member1 "Includes" "Value" Range.includes $(docs 'Range.includes)
    , member1 "Contains" "Other" Range.contains $(docs 'Range.contains)
    , negateSelf
    , absSelf Range.abs
    , floatTimes
    , plusSelf
    , plus @Angle Self
    , minusSelf
    , minus @Angle Self
    , timesFloat
    , times @(Range Unitless) Self
    , divByFloat
    , divBySelf
    , divBy @Angle Self
    , divBy @(Range Unitless) Self
    ]

color :: Class
color =
  Class.new @Color $(docs ''Color) $
    [ factory3 "RGB" "Red" "Green" "Blue" Color.rgb $(docs 'Color.rgb)
    , factory3 "RGB 255" "Red" "Green" "Blue" Color.rgb255 $(docs 'Color.rgb255)
    , factory3 "HSL" "Hue" "Saturation" "Lightness" Color.hsl $(docs 'Color.hsl)
    , factory1 "From Hex" "Hex String" Color.fromHex $(docs 'Color.fromHex)
    , member0 "To Hex" Color.toHex $(docs 'Color.toHex)
    , member0 "Components" Color.components $(docs 'Color.components)
    , member0 "Components 255" Color.components255 $(docs 'Color.components255)
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
    , factory2 "XY" "X Component" "Y Component" Vector2d.xy $(docs 'Vector2d.xy)
    , factory1 "Y" "Y Component" Vector2d.y $(docs 'Vector2d.y)
    , factory1 "X" "X Component" Vector2d.x $(docs 'Vector2d.x)
    , factory2 "Polar" "Magnitude" "Angle" Vector2d.polar $(docs 'Vector2d.polar)
    , factory1 "From Components" "Components" Vector2d.fromComponents $(docs 'Vector2d.fromComponents)
    , member0 "Components" Vector2d.components $(docs 'Vector2d.components)
    , member0 "X Component" Vector2d.xComponent $(docs 'Vector2d.xComponent)
    , member0 "Y Component" Vector2d.yComponent $(docs 'Vector2d.yComponent)
    , memberU0 "Direction" Vector2d.direction $(docs 'Vector2d.direction)
    , member0 "Angle" Vector2d.angle $(docs 'Vector2d.angle)
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
    , dot @(Vector2d (Space @ Meters)) Self
    , dot @(Vector2d (Space @ SquareMeters)) Self
    , dot @(Direction2d Space) Self
    , crossSelf
    , cross @(Vector2d (Space @ Meters)) Self
    , cross @(Vector2d (Space @ SquareMeters)) Self
    , cross @(Direction2d Space) Self
    ]

displacement2d :: Class
displacement2d =
  Class.new @(Vector2d (Space @ Meters)) "A displacement vector in 2D." $
    [ constant "Zero" (Vector2d.zero @Space @Meters) $(docs 'Vector2d.zero)
    , factory2 "XY" "X Component" "Y Component" Vector2d.xy $(docs 'Vector2d.xy)
    , factory1 "X" "X Component" Vector2d.x $(docs 'Vector2d.x)
    , factory1 "Y" "Y Component" Vector2d.y $(docs 'Vector2d.y)
    , factory2 "Polar" "Magnitude" "Angle" Vector2d.polar $(docs 'Vector2d.polar)
    , factory2 "Meters" "X Component" "Y Component" Vector2d.meters $(docs 'Vector2d.meters)
    , factory2 "Centimeters" "X Component" "Y Component" Vector2d.centimeters $(docs 'Vector2d.centimeters)
    , factory2 "Millimeters" "X Component" "Y Component" Vector2d.millimeters $(docs 'Vector2d.millimeters)
    , factory2 "Inches" "X Component" "Y Component" Vector2d.inches $(docs 'Vector2d.inches)
    , factory1 "From Components" "Components" Vector2d.fromComponents $(docs 'Vector2d.fromComponents)
    , member0 "Components" Vector2d.components $(docs 'Vector2d.components)
    , member0 "X Component" Vector2d.xComponent $(docs 'Vector2d.xComponent)
    , member0 "Y Component" Vector2d.yComponent $(docs 'Vector2d.yComponent)
    , memberM0 "Direction" Vector2d.direction $(docs 'Vector2d.direction)
    , member0 "Angle" Vector2d.angle $(docs 'Vector2d.angle)
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
    , dot @(Vector2d (Space @ Unitless)) Self
    , dot @(Direction2d Space) Self
    , crossSelf
    , cross @(Vector2d (Space @ Unitless)) Self
    , cross @(Direction2d Space) Self
    ]

areaVector2d :: Class
areaVector2d =
  Class.new @(Vector2d (Space @ SquareMeters)) "A vector in 2D with units of area." $
    [ constant "Zero" (Vector2d.zero @Space @SquareMeters) $(docs 'Vector2d.zero)
    , factory2 "XY" "X Component" "Y Component" Vector2d.xy $(docs 'Vector2d.xy)
    , factory1 "X" "X Component" Vector2d.x $(docs 'Vector2d.x)
    , factory1 "Y" "Y Component" Vector2d.y $(docs 'Vector2d.y)
    , factory2 "Polar" "Magnitude" "Angle" Vector2d.polar $(docs 'Vector2d.polar)
    , factory2 "Square Meters" "X Component" "Y Component" Vector2d.squareMeters $(docs 'Vector2d.squareMeters)
    , factory1 "From Components" "Components" Vector2d.fromComponents $(docs 'Vector2d.fromComponents)
    , member0 "Components" Vector2d.components $(docs 'Vector2d.components)
    , member0 "X Component" Vector2d.xComponent $(docs 'Vector2d.xComponent)
    , member0 "Y Component" Vector2d.yComponent $(docs 'Vector2d.yComponent)
    , memberSM0 "Direction" Vector2d.direction $(docs 'Vector2d.direction)
    , member0 "Angle" Vector2d.angle $(docs 'Vector2d.angle)
    , memberSM0 "Is Zero" (~= Vector2d.zero) "Check if an area vector is zero, within the current tolerance."
    , negateSelf
    , floatTimes
    , plusSelf
    , minusSelf
    , timesFloat
    , divByFloat
    , divBy @Length Self
    , divBy @Area Self
    , dot @(Vector2d (Space @ Unitless)) Self
    , dot @(Direction2d Space) Self
    , cross @(Vector2d (Space @ Unitless)) Self
    , cross @(Direction2d Space) Self
    ]

direction2d :: Class
direction2d =
  Class.new @(Direction2d Space) $(docs ''Direction2d) $
    [ constant "X" Direction2d.x $(docs 'Direction2d.x)
    , constant "Y" Direction2d.y $(docs 'Direction2d.y)
    , constant "Positive X" Direction2d.positiveX $(docs 'Direction2d.positiveX)
    , constant "Positive Y" Direction2d.positiveY $(docs 'Direction2d.positiveY)
    , constant "Negative X" Direction2d.negativeX $(docs 'Direction2d.negativeX)
    , constant "Negative Y" Direction2d.negativeY $(docs 'Direction2d.negativeY)
    , factory1 "From Angle" "Angle" Direction2d.fromAngle $(docs 'Direction2d.fromAngle)
    , factory1 "Degrees" "Value" Direction2d.degrees $(docs 'Direction2d.degrees)
    , factory1 "Radians" "Value" Direction2d.radians $(docs 'Direction2d.radians)
    , member0 "To Angle" Direction2d.toAngle $(docs 'Direction2d.toAngle)
    , member1 "Angle To" "Other" Direction2d.angleFrom $(docs 'Direction2d.angleFrom)
    , member0 "Components" Direction2d.components $(docs 'Direction2d.components)
    , member0 "X Component" Direction2d.xComponent $(docs 'Direction2d.xComponent)
    , member0 "Y Component" Direction2d.yComponent $(docs 'Direction2d.yComponent)
    , negateSelf
    , floatTimes
    , timesFloat
    , times @Length Self
    , times @Area Self
    , dotSelf
    , dot @(Vector2d (Space @ Unitless)) Self
    , dot @(Vector2d (Space @ Meters)) Self
    , dot @(Vector2d (Space @ SquareMeters)) Self
    , crossSelf
    , cross @(Vector2d (Space @ Unitless)) Self
    , cross @(Vector2d (Space @ Meters)) Self
    , cross @(Vector2d (Space @ SquareMeters)) Self
    ]

point2d :: Class
point2d =
  Class.new @(Point2d (Space @ Meters)) "A point in 2D, defined by its X and Y coordinates." $
    [ constant "Origin" (Point2d.origin @Space @Meters) $(docs 'Point2d.origin)
    , factory2 "XY" "X Coordinate" "Y Coordinate" Point2d.xy $(docs 'Point2d.xy)
    , factory1 "X" "X Coordinate" Point2d.x $(docs 'Point2d.x)
    , factory1 "Y" "Y Coordinate" Point2d.y $(docs 'Point2d.y)
    , factory2 "Meters" "X Coordinate" "Y Coordinate" Point2d.meters $(docs 'Point2d.meters)
    , factory2 "Centimeters" "X Coordinate" "Y Coordinate" Point2d.centimeters $(docs 'Point2d.centimeters)
    , factory2 "Millimeters" "X Coordinate" "Y Coordinate" Point2d.millimeters $(docs 'Point2d.millimeters)
    , factory2 "Inches" "X Coordinate" "Y Coordinate" Point2d.inches $(docs 'Point2d.inches)
    , factory1 "From Coordinates" "Coordinates" Point2d.fromCoordinates $(docs 'Point2d.fromCoordinates)
    , member0 "Coordinates" Point2d.coordinates $(docs 'Point2d.coordinates)
    , member0 "X Coordinate" Point2d.xCoordinate $(docs 'Point2d.xCoordinate)
    , member0 "Y Coordinate" Point2d.yCoordinate $(docs 'Point2d.yCoordinate)
    , member1 "Distance To" "Other" Point2d.distanceFrom $(docs 'Point2d.distanceFrom)
    , member1 "Midpoint" "Other" Point2d.midpoint $(docs 'Point2d.midpoint)
    , minusSelf
    , minus @(Vector2d (Space @ Meters)) Self
    , plus @(Vector2d (Space @ Meters)) Self
    , minus @(Curve2d (Space @ Meters)) Self
    ]

uvPoint :: Class
uvPoint =
  Class.new @(Point2d (Space @ Unitless)) "A point in UV parameter space." $
    [ constant "Origin" (Point2d.origin @Space @Unitless) $(docs 'Point2d.origin)
    , factory2 "UV" "U Coordinate" "V Coordinate" Point2d.xy $(docs 'Point2d.xy)
    , factory1 "U" "U Coordinate" Point2d.x $(docs 'Point2d.x)
    , factory1 "V" "V Coordinate" Point2d.y $(docs 'Point2d.y)
    , factory1 "From Coordinates" "Coordinates" Point2d.fromCoordinates $(docs 'Point2d.fromCoordinates)
    , member0 "Coordinates" Point2d.coordinates "Get the U and V coordinates of a point."
    , member0 "U Coordinate" Point2d.xCoordinate "Get the U coordinate of a point."
    , member0 "V Coordinate" Point2d.yCoordinate "Get the V coordinate of a point."
    , member1 "Distance To" "Other" Point2d.distanceFrom $(docs 'Point2d.distanceFrom)
    , member1 "Midpoint" "Other" Point2d.midpoint $(docs 'Point2d.midpoint)
    , minusSelf
    , minus @(Vector2d (Space @ Unitless)) Self
    , plus @(Vector2d (Space @ Unitless)) Self
    ]

bounds2d :: Class
bounds2d =
  Class.new @(Bounds2d (Space @ Meters)) "A bounding box in 2D." $
    [ factory2 "XY" "X Coordinate" "Y Coordinate" Bounds2d.xy $(docs 'Bounds2d.xy)
    , factory1 "Constant" "Point" Bounds2d.constant $(docs 'Bounds2d.constant)
    , factory2 "From Corners" "P1" "P2" Bounds2d.hull2 $(docs 'Bounds2d.hull2)
    , factory1 "Hull" "Points" Bounds2d.hullN $(docs 'Bounds2d.hullN)
    , factory1 "Aggregate" "Bounds" Bounds2d.aggregateN $(docs 'Bounds2d.aggregateN)
    , member0 "Coordinates" Bounds2d.coordinates $(docs 'Bounds2d.coordinates)
    , member0 "X Coordinate" Bounds2d.xCoordinate $(docs 'Bounds2d.xCoordinate)
    , member0 "Y Coordinate" Bounds2d.yCoordinate $(docs 'Bounds2d.yCoordinate)
    , plus @(Vector2d (Space @ Meters)) Self
    , minus @(Vector2d (Space @ Meters)) Self
    ]

uvBounds :: Class
uvBounds =
  Class.new @(Bounds2d (Space @ Unitless)) "A bounding box in UV parameter space." $
    [ factory2 "UV" "U Coordinate" "V Coordinate" Bounds2d.xy $(docs 'Bounds2d.xy)
    , factory1 "Constant" "Point" Bounds2d.constant $(docs 'Bounds2d.constant)
    , factory2 "From Corners" "P1" "P2" Bounds2d.hull2 $(docs 'Bounds2d.hull2)
    , factory1 "Hull" "Points" Bounds2d.hullN $(docs 'Bounds2d.hullN)
    , factory1 "Aggregate" "Bounds" Bounds2d.aggregateN $(docs 'Bounds2d.aggregateN)
    , member0 "Coordinates" Bounds2d.coordinates $(docs 'Bounds2d.coordinates)
    , member0 "U Coordinate" Bounds2d.xCoordinate $(docs 'Bounds2d.xCoordinate)
    , member0 "V Coordinate" Bounds2d.yCoordinate $(docs 'Bounds2d.yCoordinate)
    , plus @(Vector2d (Space @ Unitless)) Self
    , minus @(Vector2d (Space @ Unitless)) Self
    ]

curve :: Class
curve =
  Class.new @(Curve Unitless) "A parametric curve definining a unitless value in terms of a parameter value." $
    [ constant "Zero" (Curve.zero @Unitless) $(docs 'Curve.zero)
    , constant "T" Curve.t $(docs 'Curve.t)
    , factory1 "Constant" "Value" Curve.constant $(docs 'Curve.constant)
    , member0 "Squared" Curve.squared $(docs 'Curve.squared)
    , member0 "Sqrt" Curve.sqrt $(docs 'Curve.sqrt)
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
        [ member0 "Location" Curve.Zero.location $(docs 'Curve.Zero.location)
        , member0 "Order" Curve.Zero.order $(docs 'Curve.Zero.order)
        , member0 "Sign" ((1 *) . Curve.Zero.sign) $(docs 'Curve.Zero.sign)
        ]
    ]

angleCurve :: Class
angleCurve =
  Class.new @(Curve Radians) "A parametric curve definining an angle in terms of a parameter value." $
    [ constant "Zero" (Curve.zero @Radians) $(docs 'Curve.zero)
    , factory1 "Constant" "Value" Curve.constant $(docs 'Curve.constant)
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
    , member1 "Evaluate" "Parameter Value" (flip Curve.evaluate) $(docs 'Curve.evaluate)
    , memberSM0 "Zeros" Curve.zeros $(docs 'Curve.zeros)
    , memberSM0 "Is Zero" (~= Area.zero) "Check if a curve is zero everywhere, within the current tolerance."
    , negateSelf
    , floatTimes
    , plusSelf
    , minusSelf
    , timesFloat
    , times @(Curve Unitless) Self
    , divByFloat
    , divBySelf
    , divBy @Length Self
    , divBy @Area Self
    , divBy @(Curve Unitless) Self
    , divBy @(Curve Meters) Self
    ]

data Drawing2d_

instance FFI Drawing2d_ where
  representation = FFI.classRepresentation "Drawing2d"

drawing2d :: Class
drawing2d =
  Class.new @Drawing2d_ "A set of functions for constructing 2D drawings." $
    [ static2 "To SVG" "View Box" "Entities" Drawing2d.toSvg $(docs 'Drawing2d.toSvg)
    , static2 "Polygon" "Attributes" "Vertices" Drawing2d.polygon $(docs 'Drawing2d.polygon)
    , static3 "Circle" "Attributes" "Radius" "Center Point" Drawing2d.circle $(docs 'Drawing2d.circle)
    , static3 "Curve" "Attributes" "Max Error" "Curve" Drawing2d.curve $(docs 'Drawing2d.curve)
    , constant "Black Stroke" Drawing2d.blackStroke $(docs 'Drawing2d.blackStroke)
    , static1 "Stroke Color" "Color" Drawing2d.strokeColor $(docs 'Drawing2d.strokeColor)
    , constant "No Fill" Drawing2d.noFill $(docs 'Drawing2d.noFill)
    , static1 "Fill Color" "Color" Drawing2d.fillColor $(docs 'Drawing2d.fillColor)
    , nested @(Drawing2d.Entity Space) "A drawing entity such as a shape or group." []
    , nested @(Drawing2d.Attribute Space) "A drawing attribute such as fill color or stroke width." []
    ]

axis2d :: Class
axis2d =
  Class.new @(Axis2d (Space @ Meters)) $(docs ''Axis2d) $
    [ constant "X" (Axis2d.x @Space @Meters) $(docs 'Axis2d.x)
    , constant "Y" (Axis2d.y @Space @Meters) $(docs 'Axis2d.y)
    ]
      + orthonormalTransformations2d Axis2d.transformBy

uvAxis :: Class
uvAxis =
  Class.new @(Axis2d (Space @ Unitless)) $(docs ''Axis2d) $
    [ constant "U" (Axis2d.x @Space @Meters) "The U axis."
    , constant "V" (Axis2d.y @Space @Meters) "The V axis."
    ]

vector3d :: Class
vector3d =
  Class.new @(Vector3d (Space @ Unitless)) "A unitless vector in 3D." $
    [ constant "Zero" (Vector3d.zero @Space @Unitless) $(docs 'Vector3d.zero)
    , factory1 "Unit" "Direction" Vector3d.unit $(docs 'Vector3d.unit)
    , factory3 "XYZ" "X Component" "Y Component" "Z Component" Vector3d.xyz $(docs 'Vector3d.xyz)
    , factory1 "X" "X Component" Vector3d.x $(docs 'Vector3d.x)
    , factory1 "Y" "Y Component" Vector3d.y $(docs 'Vector3d.y)
    , factory1 "Z" "Z Component" Vector3d.z $(docs 'Vector3d.z)
    , factory1 "From Components" "Components" Vector3d.fromComponents $(docs 'Vector3d.fromComponents)
    , member0 "Components" Vector3d.components $(docs 'Vector3d.components)
    , member0 "X Component" Vector3d.xComponent $(docs 'Vector3d.xComponent)
    , member0 "Y Component" Vector3d.yComponent $(docs 'Vector3d.yComponent)
    , member0 "Z Component" Vector3d.zComponent $(docs 'Vector3d.zComponent)
    , memberU0 "Direction" Vector3d.direction $(docs 'Vector3d.direction)
    , memberU0 "Is Zero" (~= Vector3d.zero) "Check if a vector is zero, within the current tolerance."
    , member2 "Rotate In" "Direction" "Angle" Vector3d.rotateIn $(docs 'Vector3d.rotateIn)
    , member2 "Rotate Around" "Axis" "Angle" (Vector3d.rotateAround @Space @Meters) $(docs 'Vector3d.rotateAround)
    , member1 "Mirror In" "Direction" Vector3d.mirrorIn $(docs 'Vector3d.mirrorIn)
    , member1 "Mirror Across" "Plane" (Vector3d.mirrorAcross @Space @Meters) $(docs 'Vector3d.mirrorAcross)
    , member2 "Scale In" "Direction" "Scale" Vector3d.scaleIn $(docs 'Vector3d.scaleIn)
    , member2 "Scale Along" "Axis" "Scale" (Vector3d.scaleAlong @Space @Meters) $(docs 'Vector3d.scaleAlong)
    , negateSelf
    , floatTimes
    , plusSelf
    , minusSelf
    , timesFloat
    , times @Length Self
    , times @Area Self
    , divByFloat
    , dotSelf
    , dot @(Vector3d (Space @ Meters)) Self
    , dot @(Vector3d (Space @ SquareMeters)) Self
    , dot @(Direction3d Space) Self
    , crossSelf
    , cross @(Vector3d (Space @ Meters)) Self
    , cross @(Vector3d (Space @ SquareMeters)) Self
    , cross @(Direction3d Space) Self
    ]

displacement3d :: Class
displacement3d =
  Class.new @(Vector3d (Space @ Meters)) "A displacement vector in 3D." $
    [ constant "Zero" (Vector3d.zero @Space @Meters) $(docs 'Vector3d.zero)
    , factory3 "XYZ" "X Component" "Y Component" "Z Component" Vector3d.xyz $(docs 'Vector3d.xyz)
    , factory1 "X" "X Component" Vector3d.x $(docs 'Vector3d.x)
    , factory1 "Y" "Y Component" Vector3d.y $(docs 'Vector3d.y)
    , factory1 "Z" "Z Component" Vector3d.z $(docs 'Vector3d.z)
    , factory3 "Meters" "X Component" "Y Component" "Z Component" Vector3d.meters $(docs 'Vector3d.meters)
    , factory3 "Centimeters" "X Component" "Y Component" "Z Component" Vector3d.centimeters $(docs 'Vector3d.centimeters)
    , factory3 "Millimeters" "X Component" "Y Component" "Z Component" Vector3d.millimeters $(docs 'Vector3d.millimeters)
    , factory3 "Inches" "X Component" "Y Component" "Z Component" Vector3d.inches $(docs 'Vector3d.inches)
    , factory1 "From Components" "Components" Vector3d.fromComponents $(docs 'Vector3d.fromComponents)
    , member0 "Components" Vector3d.components $(docs 'Vector3d.components)
    , member0 "X Component" Vector3d.xComponent $(docs 'Vector3d.xComponent)
    , member0 "Y Component" Vector3d.yComponent $(docs 'Vector3d.yComponent)
    , member0 "Z Component" Vector3d.zComponent $(docs 'Vector3d.zComponent)
    , memberM0 "Direction" Vector3d.direction $(docs 'Vector3d.direction)
    , memberM0 "Is Zero" (~= Vector3d.zero) "Check if a displacement is zero, within the current tolerance."
    , member2 "Rotate In" "Direction" "Angle" Vector3d.rotateIn $(docs 'Vector3d.rotateIn)
    , member2 "Rotate Around" "Axis" "Angle" (Vector3d.rotateAround @Space @Meters) $(docs 'Vector3d.rotateAround)
    , member1 "Mirror In" "Direction" Vector3d.mirrorIn $(docs 'Vector3d.mirrorIn)
    , member1 "Mirror Across" "Plane" (Vector3d.mirrorAcross @Space @Meters) $(docs 'Vector3d.mirrorAcross)
    , member2 "Scale In" "Direction" "Scale" Vector3d.scaleIn $(docs 'Vector3d.scaleIn)
    , member2 "Scale Along" "Axis" "Scale" (Vector3d.scaleAlong @Space @Meters) $(docs 'Vector3d.scaleAlong)
    , negateSelf
    , floatTimes
    , plusSelf
    , minusSelf
    , timesFloat
    , times @Length Self
    , divByFloat
    , divBy @Length Self
    , dotSelf
    , dot @(Vector3d (Space @ Unitless)) Self
    , dot @(Direction3d Space) Self
    , crossSelf
    , cross @(Vector3d (Space @ Unitless)) Self
    , cross @(Direction3d Space) Self
    ]

areaVector3d :: Class
areaVector3d =
  Class.new @(Vector3d (Space @ SquareMeters)) "A vector in 3D with units of area." $
    [ constant "Zero" (Vector3d.zero @Space @SquareMeters) $(docs 'Vector3d.zero)
    , factory3 "XYZ" "X Component" "Y Component" "Z Component" Vector3d.xyz $(docs 'Vector3d.xyz)
    , factory1 "X" "X Component" Vector3d.x $(docs 'Vector3d.x)
    , factory1 "Y" "Y Component" Vector3d.y $(docs 'Vector3d.y)
    , factory1 "Z" "Z Component" Vector3d.z $(docs 'Vector3d.z)
    , factory3 "Square Meters" "X Component" "Y Component" "Z Component" Vector3d.squareMeters $(docs 'Vector3d.squareMeters)
    , factory1 "From Components" "Components" Vector3d.fromComponents $(docs 'Vector3d.fromComponents)
    , member0 "Components" Vector3d.components $(docs 'Vector3d.components)
    , member0 "X Component" Vector3d.xComponent $(docs 'Vector3d.xComponent)
    , member0 "Y Component" Vector3d.yComponent $(docs 'Vector3d.yComponent)
    , member0 "Z Component" Vector3d.zComponent $(docs 'Vector3d.zComponent)
    , memberSM0 "Direction" Vector3d.direction $(docs 'Vector3d.direction)
    , memberSM0 "Is Zero" (~= Vector3d.zero) "Check if an area vector is zero, within the current tolerance."
    , member2 "Rotate In" "Direction" "Angle" Vector3d.rotateIn $(docs 'Vector3d.rotateIn)
    , member2 "Rotate Around" "Axis" "Angle" (Vector3d.rotateAround @Space @Meters) $(docs 'Vector3d.rotateAround)
    , member1 "Mirror In" "Direction" Vector3d.mirrorIn $(docs 'Vector3d.mirrorIn)
    , member1 "Mirror Across" "Plane" (Vector3d.mirrorAcross @Space @Meters) $(docs 'Vector3d.mirrorAcross)
    , member2 "Scale In" "Direction" "Scale" Vector3d.scaleIn $(docs 'Vector3d.scaleIn)
    , member2 "Scale Along" "Axis" "Scale" (Vector3d.scaleAlong @Space @Meters) $(docs 'Vector3d.scaleAlong)
    , negateSelf
    , floatTimes
    , plusSelf
    , minusSelf
    , timesFloat
    , divByFloat
    , divBy @Length Self
    , divBy @Area Self
    , dot @(Vector3d (Space @ Unitless)) Self
    , dot @(Direction3d Space) Self
    , cross @(Vector3d (Space @ Unitless)) Self
    , cross @(Direction3d Space) Self
    ]

direction3d :: Class
direction3d =
  Class.new @(Direction3d Space) $(docs ''Direction3d) $
    [ constant "X" Direction3d.x $(docs 'Direction3d.x)
    , constant "Y" Direction3d.y $(docs 'Direction3d.y)
    , constant "Z" Direction3d.z $(docs 'Direction3d.z)
    , constant "Positive X" Direction3d.positiveX $(docs 'Direction3d.positiveX)
    , constant "Positive Y" Direction3d.positiveY $(docs 'Direction3d.positiveY)
    , constant "Positive Z" Direction3d.positiveZ $(docs 'Direction3d.positiveZ)
    , constant "Negative X" Direction3d.negativeX $(docs 'Direction3d.negativeX)
    , constant "Negative Y" Direction3d.negativeY $(docs 'Direction3d.negativeY)
    , constant "Negative Z" Direction3d.negativeZ $(docs 'Direction3d.negativeZ)
    , member1 "Angle To" "Other" Direction3d.angleFrom $(docs 'Direction3d.angleFrom)
    , member0 "Components" Direction3d.components $(docs 'Direction3d.components)
    , member0 "X Component" Direction3d.xComponent $(docs 'Direction3d.xComponent)
    , member0 "Y Component" Direction3d.yComponent $(docs 'Direction3d.yComponent)
    , member0 "Z Component" Direction3d.zComponent $(docs 'Direction3d.zComponent)
    , member2 "Rotate In" "Direction" "Angle" Direction3d.rotateIn $(docs 'Direction3d.rotateIn)
    , member2 "Rotate Around" "Axis" "Angle" (Direction3d.rotateAround @Space @Meters) $(docs 'Direction3d.rotateAround)
    , member1 "Mirror In" "Direction" Direction3d.mirrorIn $(docs 'Direction3d.mirrorIn)
    , member1 "Mirror Across" "Plane" (Direction3d.mirrorAcross @Space @Meters) $(docs 'Direction3d.mirrorAcross)
    , negateSelf
    , floatTimes
    , timesFloat
    , times @Length Self
    , times @Area Self
    , dotSelf
    , dot @(Vector3d (Space @ Unitless)) Self
    , dot @(Vector3d (Space @ Meters)) Self
    , dot @(Vector3d (Space @ SquareMeters)) Self
    , crossSelf
    , cross @(Vector3d (Space @ Unitless)) Self
    , cross @(Vector3d (Space @ Meters)) Self
    , cross @(Vector3d (Space @ SquareMeters)) Self
    ]

point3d :: Class
point3d =
  Class.new @(Point3d (Space @ Meters)) "A point in 3D, defined by its XYZ coordinates." $
    [ constant "Origin" (Point3d.origin @Space @Meters) $(docs 'Point3d.origin)
    , factory3 "XYZ" "X Coordinate" "Y Coordinate" "Z Coordinate" Point3d.xyz $(docs 'Point3d.xyz)
    , factory1 "X" "X Coordinate" Point3d.x $(docs 'Point3d.x)
    , factory1 "Y" "Y Coordinate" Point3d.y $(docs 'Point3d.y)
    , factory1 "Z" "Z Coordinate" Point3d.z $(docs 'Point3d.z)
    , factory3 "Meters" "X Coordinate" "Y Coordinate" "Z Coordinate" Point3d.meters $(docs 'Point3d.meters)
    , factory3 "Centimeters" "X Coordinate" "Y Coordinate" "Z Coordinate" Point3d.centimeters $(docs 'Point3d.centimeters)
    , factory3 "Millimeters" "X Coordinate" "Y Coordinate" "Z Coordinate" Point3d.millimeters $(docs 'Point3d.millimeters)
    , factory3 "Inches" "X Coordinate" "Y Coordinate" "Z Coordinate" Point3d.inches $(docs 'Point3d.inches)
    , factory1 "From Coordinates" "Coordinates" Point3d.fromCoordinates $(docs 'Point3d.fromCoordinates)
    , member0 "Coordinates" Point3d.coordinates $(docs 'Point3d.coordinates)
    , member0 "X Coordinate" Point3d.xCoordinate $(docs 'Point3d.xCoordinate)
    , member0 "Y Coordinate" Point3d.yCoordinate $(docs 'Point3d.yCoordinate)
    , member0 "Z Coordinate" Point3d.zCoordinate $(docs 'Point3d.zCoordinate)
    , member1 "Distance To" "Other" Point3d.distanceFrom $(docs 'Point3d.distanceFrom)
    , member1 "Midpoint" "Other" Point3d.midpoint $(docs 'Point3d.midpoint)
    , minusSelf
    , minus @(Vector3d (Space @ Meters)) Self
    , plus @(Vector3d (Space @ Meters)) Self
    ]
      + affineTransformations3d Point3d.transformBy

bounds3d :: Class
bounds3d =
  Class.new @(Bounds3d (Space @ Meters)) "A bounding box in 3D." $
    [ factory3 "XYZ" "X Coordinate" "Y Coordinate" "Z Coordinate" Bounds3d.xyz $(docs 'Bounds3d.xyz)
    , factory1 "Constant" "Point" Bounds3d.constant $(docs 'Bounds3d.constant)
    , factory2 "From Corners" "P1" "P2" Bounds3d.hull2 $(docs 'Bounds3d.hull2)
    , factory1 "Hull" "Points" Bounds3d.hullN $(docs 'Bounds3d.hullN)
    , factory1 "Aggregate" "Bounds" Bounds3d.aggregateN $(docs 'Bounds3d.aggregateN)
    , member0 "Coordinates" Bounds3d.coordinates $(docs 'Bounds3d.coordinates)
    , member0 "X Coordinate" Bounds3d.xCoordinate $(docs 'Bounds3d.xCoordinate)
    , member0 "Y Coordinate" Bounds3d.yCoordinate $(docs 'Bounds3d.yCoordinate)
    , member0 "Z Coordinate" Bounds3d.zCoordinate $(docs 'Bounds3d.zCoordinate)
    , plus @(Vector3d (Space @ Meters)) Self
    , minus @(Vector3d (Space @ Meters)) Self
    ]
      + affineTransformations3d Bounds3d.transformBy

axis3d :: Class
axis3d =
  Class.new @(Axis3d (Space @ Meters)) $(docs ''Axis3d) $
    [ constant "X" (Axis3d.x @Space @Meters) $(docs 'Axis3d.x)
    , constant "Y" (Axis3d.y @Space @Meters) $(docs 'Axis3d.y)
    , constant "Z" (Axis3d.z @Space @Meters) $(docs 'Axis3d.z)
    , member0 "Origin Point" Axis3d.originPoint $(docs 'Axis3d.originPoint)
    , member0 "Direction" Axis3d.direction $(docs 'Axis3d.direction)
    , member0 "Normal Plane" Axis3d.normalPlane $(docs 'Axis3d.normalPlane)
    , member1 "Move To" "Point" Axis3d.moveTo $(docs 'Axis3d.moveTo)
    , member0 "Reverse" Axis3d.reverse $(docs 'Axis3d.reverse)
    ]
      + orthonormalTransformations3d Axis3d.transformBy

plane3d :: Class
plane3d =
  Class.new @(Plane3d (Space @ Meters) (Defines Space)) $(docs ''Plane3d) $
    [ constant "XY" (Plane3d.xy @Space @Meters) $(docs 'Plane3d.xy)
    , constant "YX" (Plane3d.yx @Space @Meters) $(docs 'Plane3d.yx)
    , constant "ZX" (Plane3d.zx @Space @Meters) $(docs 'Plane3d.zx)
    , constant "XZ" (Plane3d.xz @Space @Meters) $(docs 'Plane3d.xz)
    , constant "YZ" (Plane3d.yz @Space @Meters) $(docs 'Plane3d.yz)
    , constant "ZY" (Plane3d.zy @Space @Meters) $(docs 'Plane3d.zy)
    , member0 "Origin Point" Plane3d.originPoint $(docs 'Plane3d.originPoint)
    , member0 "Normal Direction" Plane3d.normalDirection $(docs 'Plane3d.normalDirection)
    , member0 "Normal Axis" Plane3d.normalAxis $(docs 'Plane3d.normalAxis)
    , member0 "X Direction" Plane3d.xDirection $(docs 'Plane3d.xDirection)
    , member0 "Y Direction" Plane3d.yDirection $(docs 'Plane3d.yDirection)
    , member1 "Move To" "Point" Plane3d.moveTo $(docs 'Plane3d.moveTo)
    ]
      + orthonormalTransformations3d Plane3d.transformBy

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
    , factory4 "Polar Arc" "Center Point" "Radius" "Start Angle" "End Angle" Curve2d.polarArc $(docs 'Curve2d.polarArc)
    , factory3 "Swept Arc" "Center Point" "Start Point" "Swept Angle" Curve2d.sweptArc $(docs 'Curve2d.sweptArc)
    , factoryM4 "Corner Arc" "Corner Point" "Incoming Direction" "Outgoing Direction" "Radius" Curve2d.cornerArc $(docs 'Curve2d.cornerArc)
    , factory1 "Bezier" "Control Points" Curve2d.bezier $(docs 'Curve2d.bezier)
    , factory4 "Hermite" "Start Point" "Start Derivatives" "End Point" "End Derivatives" Curve2d.hermite $(docs 'Curve2d.hermite)
    , member0 "Start Point" Curve2d.startPoint $(docs 'Curve2d.startPoint)
    , member0 "End Point" Curve2d.endPoint $(docs 'Curve2d.endPoint)
    , member1 "Evaluate" "Parameter Value" (flip Curve2d.evaluate) $(docs 'Curve2d.evaluate)
    , member0 "Derivative" Curve2d.derivative $(docs 'Curve2d.derivative)
    , member0 "Reverse" Curve2d.reverse $(docs 'Curve2d.reverse)
    , member0 "X Coordinate" Curve2d.xCoordinate $(docs 'Curve2d.xCoordinate)
    , member0 "Y Coordinate" Curve2d.yCoordinate $(docs 'Curve2d.yCoordinate)
    , plus @(VectorCurve2d (Space @ Meters)) Self
    , minus @(VectorCurve2d (Space @ Meters)) Self
    , minusSelf
    , minus @(Point2d (Space @ Meters)) Self
    ]
      + affineTransformations2d Curve2d.transformBy

uvCurve :: Class
uvCurve =
  Class.new @(Curve2d (Space @ Unitless)) $(docs ''Curve2d) $
    [ factory1 "Constant" "Point" Curve2d.constant $(docs 'Curve2d.constant)
    , factory2 "UV" "U Coordinate" "V Coordinate" Curve2d.xy $(docs 'Curve2d.xy)
    , factory2 "Line" "Start Point" "End Point" Curve2d.line $(docs 'Curve2d.line)
    , factoryU3 "Arc" "Start Point" "End Point" "Swept Angle" Curve2d.arc $(docs 'Curve2d.arc)
    , factory4 "Polar Arc" "Center Point" "Radius" "Start Angle" "End Angle" Curve2d.polarArc $(docs 'Curve2d.polarArc)
    , factory3 "Swept Arc" "Center Point" "Start Point" "Swept Angle" Curve2d.sweptArc $(docs 'Curve2d.sweptArc)
    , factoryU4 "Corner Arc" "Corner Point" "Incoming Direction" "Outgoing Direction" "Radius" Curve2d.cornerArc $(docs 'Curve2d.cornerArc)
    , factory1 "Bezier" "Control Points" Curve2d.bezier $(docs 'Curve2d.bezier)
    , factory4 "Hermite" "Start Point" "Start Derivatives" "End Point" "End Derivatives" Curve2d.hermite $(docs 'Curve2d.hermite)
    , member0 "Start Point" Curve2d.startPoint $(docs 'Curve2d.startPoint)
    , member0 "End Point" Curve2d.endPoint $(docs 'Curve2d.endPoint)
    , member1 "Evaluate" "Parameter Value" (flip Curve2d.evaluate) $(docs 'Curve2d.evaluate)
    , member0 "Derivative" Curve2d.derivative $(docs 'Curve2d.derivative)
    , member0 "Reverse" Curve2d.reverse $(docs 'Curve2d.reverse)
    , member0 "U Coordinate" Curve2d.xCoordinate $(docs 'Curve2d.xCoordinate)
    , member0 "V Coordinate" Curve2d.yCoordinate $(docs 'Curve2d.yCoordinate)
    , plus @(VectorCurve2d (Space @ Unitless)) Self
    , minus @(VectorCurve2d (Space @ Unitless)) Self
    , minusSelf
    , minus @(Point2d (Space @ Unitless)) Self
    ]
      + affineTransformations2d Curve2d.transformBy

region2d :: Class
region2d =
  Class.new @(Region2d (Space @ Meters)) $(docs ''Region2d) $
    [ factoryM1R "Bounded By" "Curves" Region2d.boundedBy $(docs 'Region2d.boundedBy)
    , factoryM1R "Rectangle" "Bounding Box" Region2d.rectangle $(docs 'Region2d.rectangle)
    , factoryM2R "Circle" "Center Pointer" "Radius" Region2d.circle $(docs 'Region2d.circle)
    , member0 "Outer Loop" Region2d.outerLoop $(docs 'Region2d.outerLoop)
    , member0 "Inner Loops" Region2d.innerLoops $(docs 'Region2d.innerLoops)
    , member0 "Boundary Curves" Region2d.boundaryCurves $(docs 'Region2d.boundaryCurves)
    ]
      + affineTransformations2d Region2d.transformBy

uvRegion :: Class
uvRegion =
  Class.new @(Region2d (Space @ Unitless)) $(docs ''Region2d) $
    [ constant "Unit" Region2d.unit $(docs 'Region2d.unit)
    , factoryU1R "Bounded By" "Curves" Region2d.boundedBy $(docs 'Region2d.boundedBy)
    , factoryU1R "Rectangle" "Bounding Box" Region2d.rectangle $(docs 'Region2d.rectangle)
    , factoryU2R "Circle" "Center Pointer" "Radius" Region2d.circle $(docs 'Region2d.circle)
    , member0 "Outer Loop" Region2d.outerLoop $(docs 'Region2d.outerLoop)
    , member0 "Inner Loops" Region2d.innerLoops $(docs 'Region2d.innerLoops)
    , member0 "Boundary Curves" Region2d.boundaryCurves $(docs 'Region2d.boundaryCurves)
    ]
      + affineTransformations2d Region2d.transformBy

body3d :: Class
body3d =
  Class.new @(Body3d (Space @ Meters)) $(docs ''Body3d) $
    [ factoryM3R "Extruded" "Sketch Plane" "Profile" "Distance" Body3d.extruded $(docs 'Body3d.extruded)
    , factoryM4R "Revolved" "Sketch Plane" "Profile" "Axis" "Angle" Body3d.revolved $(docs 'Body3d.revolved)
    , factoryM1R "Block" "Bounding Box" Body3d.block $(docs 'Body3d.block)
    , factoryM3R "Cylinder" "Start Point" "End Point" "Radius" Body3d.cylinder $(docs 'Body3d.cylinder)
    , factoryM3R "Cylinder Along" "Axis" "Distance" "Radius" Body3d.cylinderAlong $(docs 'Body3d.cylinderAlong)
    ]

data Mesh_

instance FFI Mesh_ where
  representation = FFI.classRepresentation "Mesh"

mesh :: Class
mesh =
  Class.new @Mesh_ "Meshing-related functionality." $
    [ nested @(Mesh.Constraint Meters) $(docs ''Mesh.Constraint) []
    , static1 "Max Error" "Error" (Mesh.maxError @Meters) $(docs 'Mesh.maxError)
    , static1 "Max Size" "Size" (Mesh.maxSize @Meters) $(docs 'Mesh.maxSize)
    ]

data Scene3d_

instance FFI Scene3d_ where
  representation = FFI.classRepresentation "Scene3d"

scene3d :: Class
scene3d =
  Class.new @Scene3d_ "A set of functions for constructing 3D scenes." $
    [ staticM3 "Body" "Mesh Constraints" "Material" "Body" (Scene3d.body @Space) $(docs 'Scene3d.body)
    , static1 "Group" "Entities" (Scene3d.group @Space) $(docs 'Scene3d.group)
    , static2 "Metal" "Base Color" "Roughness" Scene3d.metal $(docs 'Scene3d.metal)
    , static2 "Nonmetal" "Base Color" "Roughness" Scene3d.nonmetal $(docs 'Scene3d.nonmetal)
    , static3 "Write GLB" "Path" "Ground Plane" "Entities" (Scene3d.writeGlb @Space) $(docs 'Scene3d.writeGlb)
    , nested @(Scene3d.Entity Space) "A scene entity such as a mesh or group." []
    , nested @Scene3d.Material "A material applied to a mesh." []
    ]

functions :: List Function
functions = List.collect Class.functions classes
