module API (classes, functions) where

import API.Class
  ( Class
  , Self (Self)
  , absSelf
  , comparison
  , constant
  , constructor2
  , constructor3
  , crossProduct
  , crossSelf
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
  , static1
  , static2
  , static3
  , staticM3
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
import OpenSolid.Axis2d (Axis2d)
import OpenSolid.Axis2d qualified as Axis2d
import OpenSolid.Axis3d (Axis3d)
import OpenSolid.Axis3d qualified as Axis3d
import OpenSolid.Basis3d (Basis3d)
import OpenSolid.Basis3d qualified as Basis3d
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
import OpenSolid.Convention3d (Convention3d (Convention3d))
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
import OpenSolid.Drawing2d qualified as Drawing2d
import OpenSolid.FFI (FFI)
import OpenSolid.Frame3d (Frame3d)
import OpenSolid.Frame3d qualified as Frame3d
import OpenSolid.Length (Length)
import OpenSolid.Length qualified as Length
import OpenSolid.List qualified as List
import OpenSolid.Mesh qualified as Mesh
import OpenSolid.PlanarBasis3d (PlanarBasis3d)
import OpenSolid.PlanarBasis3d qualified as PlanarBasis3d
import OpenSolid.Plane3d (Plane3d)
import OpenSolid.Plane3d qualified as Plane3d
import OpenSolid.Point2d (Point2d (Point2d))
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Point3d (Point3d)
import OpenSolid.Point3d qualified as Point3d
import OpenSolid.Prelude
import OpenSolid.Qty qualified as Qty
import OpenSolid.Region2d (Region2d)
import OpenSolid.Region2d qualified as Region2d
import OpenSolid.Scene3d qualified as Scene3d
import OpenSolid.SpurGear (SpurGear)
import OpenSolid.SpurGear qualified as SpurGear
import OpenSolid.Stl qualified as Stl
import OpenSolid.Transform qualified as Transform
import OpenSolid.Transform2d (Transform2d)
import OpenSolid.Transform2d qualified as Transform2d
import OpenSolid.Transform3d (Transform3d)
import OpenSolid.Transform3d qualified as Transform3d
import OpenSolid.Units (Meters, Radians, SquareMeters)
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
  , planarBasis3d
  , plane3d
  , basis3d
  , frame3d
  , vectorCurve2d
  , displacementCurve2d
  , curve2d
  , uvCurve
  , region2d
  , uvRegion
  , body3d
  , mesh
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
    , member0 "Endpoints" Bounds.endpoints $(docs 'Bounds.endpoints)
    , member0 "Lower" Bounds.lower $(docs 'Bounds.lower)
    , member0 "Upper" Bounds.upper $(docs 'Bounds.upper)
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
    , member0 "Endpoints" Bounds.endpoints $(docs 'Bounds.endpoints)
    , member0 "Lower" Bounds.lower $(docs 'Bounds.lower)
    , member0 "Upper" Bounds.upper $(docs 'Bounds.upper)
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
    , member0 "Endpoints" Bounds.endpoints $(docs 'Bounds.endpoints)
    , member0 "Lower" Bounds.lower $(docs 'Bounds.lower)
    , member0 "Upper" Bounds.upper $(docs 'Bounds.upper)
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
    , member0 "Endpoints" Bounds.endpoints $(docs 'Bounds.endpoints)
    , member0 "Lower" Bounds.lower $(docs 'Bounds.lower)
    , member0 "Upper" Bounds.upper $(docs 'Bounds.upper)
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
    , constructor2 "X Component" "Y Component" Vector2d $(docs 'Vector2d)
    , factory2 "XY" "X Component" "Y Component" Vector2d $(docs 'Vector2d)
    , factory1 "Y" "Y Component" Vector2d.y $(docs 'Vector2d.y)
    , factory1 "X" "X Component" Vector2d.x $(docs 'Vector2d.x)
    , factory2 "Polar" "Magnitude" "Angle" Vector2d.polar $(docs 'Vector2d.polar)
    , factory1 "From Components" "Components" Vector2d.fromComponents $(docs 'Vector2d.fromComponents)
    , member0 "Components" Vector2d.components $(docs 'Vector2d.components)
    , member0 "X Component" Vector2d.xComponent $(docs 'Vector2d.xComponent)
    , member0 "Y Component" Vector2d.yComponent $(docs 'Vector2d.yComponent)
    , memberU0 "Direction" Vector2d.direction $(docs 'Vector2d.direction)
    , member0 "Normalize" Vector2d.normalize $(docs 'Vector2d.normalize)
    , member0 "Angle" Vector2d.angle $(docs 'Vector2d.angle)
    , member1 "Angle To" "Other" Vector2d.angleFrom $(docs 'Vector2d.angleFrom)
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
    , factory2 "XY" "X Component" "Y Component" Vector2d $(docs 'Vector2d)
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
    , member0 "Components" Vector2d.components $(docs 'Vector2d.components)
    , member0 "X Component" Vector2d.xComponent $(docs 'Vector2d.xComponent)
    , member0 "Y Component" Vector2d.yComponent $(docs 'Vector2d.yComponent)
    , memberM0 "Direction" Vector2d.direction $(docs 'Vector2d.direction)
    , member0 "Normalize" Vector2d.normalize $(docs 'Vector2d.normalize)
    , member0 "Angle" Vector2d.angle $(docs 'Vector2d.angle)
    , member1 "Angle To" "Other" Vector2d.angleFrom $(docs 'Vector2d.angleFrom)
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
    , factory2 "XY" "X Component" "Y Component" Vector2d $(docs 'Vector2d)
    , factory1 "X" "X Component" Vector2d.x $(docs 'Vector2d.x)
    , factory1 "Y" "Y Component" Vector2d.y $(docs 'Vector2d.y)
    , factory2 "Polar" "Magnitude" "Angle" Vector2d.polar $(docs 'Vector2d.polar)
    , factory2 "Square Meters" "X Component" "Y Component" Vector2d.squareMeters $(docs 'Vector2d.squareMeters)
    , factory1 "From Components" "Components" Vector2d.fromComponents $(docs 'Vector2d.fromComponents)
    , member0 "Components" Vector2d.components $(docs 'Vector2d.components)
    , member0 "X Component" Vector2d.xComponent $(docs 'Vector2d.xComponent)
    , member0 "Y Component" Vector2d.yComponent $(docs 'Vector2d.yComponent)
    , memberS0 "Direction" Vector2d.direction $(docs 'Vector2d.direction)
    , member0 "Normalize" Vector2d.normalize $(docs 'Vector2d.normalize)
    , member0 "Angle" Vector2d.angle $(docs 'Vector2d.angle)
    , member1 "Angle To" "Other" Vector2d.angleFrom $(docs 'Vector2d.angleFrom)
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
    , negateSelf
    ]

point2d :: Class
point2d =
  Class.new @(Point2d (Space @ Meters)) "A point in 2D, defined by its X and Y coordinates." $
    [ constant "Origin" (Point2d.origin @Space @Meters) $(docs 'Point2d.origin)
    , constructor2 "X Coordinate" "Y Coordinate" Point2d $(docs 'Point2d)
    , factory2 "XY" "X Coordinate" "Y Coordinate" Point2d $(docs 'Point2d)
    , factory1 "X" "X Coordinate" Point2d.x $(docs 'Point2d.x)
    , factory1 "Y" "Y Coordinate" Point2d.y $(docs 'Point2d.y)
    , factory2 "Meters" "X Coordinate" "Y Coordinate" Point2d.meters $(docs 'Point2d.meters)
    , factory2 "Centimeters" "X Coordinate" "Y Coordinate" Point2d.centimeters $(docs 'Point2d.centimeters)
    , factory2 "Cm" "X Coordinate" "Y Coordinate" Point2d.cm $(docs 'Point2d.cm)
    , factory2 "Millimeters" "X Coordinate" "Y Coordinate" Point2d.millimeters $(docs 'Point2d.millimeters)
    , factory2 "Mm" "X Coordinate" "Y Coordinate" Point2d.mm $(docs 'Point2d.mm)
    , factory2 "Inches" "X Coordinate" "Y Coordinate" Point2d.inches $(docs 'Point2d.inches)
    , factory1 "From Coordinates" "Coordinates" Point2d.fromCoordinates $(docs 'Point2d.fromCoordinates)
    , member0 "Coordinates" Point2d.coordinates $(docs 'Point2d.coordinates)
    , member0 "X Coordinate" Point2d.xCoordinate $(docs 'Point2d.xCoordinate)
    , member0 "Y Coordinate" Point2d.yCoordinate $(docs 'Point2d.yCoordinate)
    , member1 "Distance To" "Other" Point2d.distanceFrom $(docs 'Point2d.distanceFrom)
    , member1 "Midpoint" "Other" Point2d.midpoint $(docs 'Point2d.midpoint)
    , member1 "Place On" "Plane" Point2d.placeOn $(docs 'Point2d.placeOn)
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
    , member0 "Coordinates" Point2d.coordinates "Get the U and V coordinates of a point."
    , member0 "U Coordinate" Point2d.xCoordinate "Get the U coordinate of a point."
    , member0 "V Coordinate" Point2d.yCoordinate "Get the V coordinate of a point."
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
    , factory1 "Hull" "Points" Bounds2d.hullN $(docs 'Bounds2d.hullN)
    , factory1 "Aggregate" "Bounds" Bounds2d.aggregateN $(docs 'Bounds2d.aggregateN)
    , member0 "Coordinates" Bounds2d.coordinates $(docs 'Bounds2d.coordinates)
    , member0 "X Coordinate" Bounds2d.xCoordinate $(docs 'Bounds2d.xCoordinate)
    , member0 "Y Coordinate" Bounds2d.yCoordinate $(docs 'Bounds2d.yCoordinate)
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
    , factory1 "Hull" "Points" Bounds2d.hullN $(docs 'Bounds2d.hullN)
    , factory1 "Aggregate" "Bounds" Bounds2d.aggregateN $(docs 'Bounds2d.aggregateN)
    , member0 "Coordinates" Bounds2d.coordinates $(docs 'Bounds2d.coordinates)
    , member0 "U Coordinate" Bounds2d.xCoordinate "Get the U coordinate bounds of a bounding box."
    , member0 "V Coordinate" Bounds2d.yCoordinate "Get the V coordinate bounds of a bounding box."
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
  Class.static "Drawing2d" "A set of functions for constructing 2D drawings." $
    [ static2 "To SVG" "View Box" "Entities" Drawing2d.toSvg $(docs 'Drawing2d.toSvg)
    , static3 "Write SVG" "Path" "View Box" "Entities" Drawing2d.writeSvg $(docs 'Drawing2d.writeSvg)
    , static1 "Group" "Entities" Drawing2d.group $(docs 'Drawing2d.group)
    , static2 "Polygon" "Attributes" "Vertices" Drawing2d.polygon $(docs 'Drawing2d.polygon)
    , static3 "Circle" "Attributes" "Center Point" "Diameter" Drawing2d.circle $(docs 'Drawing2d.circle)
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
      <> orthonormalTransformations2d Axis2d.transformBy

uvAxis :: Class
uvAxis =
  Class.new @(Axis2d (Space @ Unitless)) $(docs ''Axis2d) $
    [ constant "U" (Axis2d.x @Space @Meters) "The U axis."
    , constant "V" (Axis2d.y @Space @Meters) "The V axis."
    ]

convention3d :: Class
convention3d = do
  let constructor ::
        Named "xDirection" (Direction3d Space) ->
        Named "yDirection" (Direction3d Space) ->
        Named "zDirection" (Direction3d Space) ->
        Convention3d Space
      constructor (Named xDirection) (Named yDirection) (Named zDirection) =
        Convention3d{xDirection, yDirection, zDirection}
  Class.new @(Convention3d Space) $(docs ''Convention3d) $
    [ constructor3 "X Direction" "Y Direction" "Z Direction" constructor "Construct a coordinate convention from given X, Y and Z directions."
    , constant "Y Up" Convention3d.yUp $(docs 'Convention3d.yUp)
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
    , member1 "Place In" "Basis" Vector3d.placeIn $(docs 'Vector3d.placeIn)
    , member1 "Relative To" "Basis" Vector3d.relativeTo $(docs 'Vector3d.relativeTo)
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
    , member1 "Place In" "Basis" Vector3d.placeIn $(docs 'Vector3d.placeIn)
    , member1 "Relative To" "Basis" Vector3d.relativeTo $(docs 'Vector3d.relativeTo)
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
    , member1 "Place In" "Basis" Vector3d.placeIn $(docs 'Vector3d.placeIn)
    , member1 "Relative To" "Basis" Vector3d.relativeTo $(docs 'Vector3d.relativeTo)
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
    , constant "Rightward" Direction3d.rightward $(docs 'Direction3d.rightward)
    , constant "Leftward" Direction3d.leftward $(docs 'Direction3d.leftward)
    , constant "Forward" Direction3d.forward $(docs 'Direction3d.forward)
    , constant "Backward" Direction3d.backward $(docs 'Direction3d.backward)
    , constant "Upward" Direction3d.upward $(docs 'Direction3d.upward)
    , constant "Downward" Direction3d.downward $(docs 'Direction3d.downward)
    , member0 "Arbitrary Perpendicular Direction" Direction3d.arbitraryPerpendicularDirection $(docs 'Direction3d.arbitraryPerpendicularDirection)
    , member0 "Arbitrary Normal Basis" Direction3d.arbitraryNormalBasis $(docs 'Direction3d.arbitraryNormalBasis)
    , member1 "Angle To" "Other" Direction3d.angleFrom $(docs 'Direction3d.angleFrom)
    , member2 "Rotate In" "Direction" "Angle" Direction3d.rotateIn $(docs 'Direction3d.rotateIn)
    , member2 "Rotate Around" "Axis" "Angle" (Direction3d.rotateAround @Space @Meters) $(docs 'Direction3d.rotateAround)
    , member1 "Mirror In" "Direction" Direction3d.mirrorIn $(docs 'Direction3d.mirrorIn)
    , member1 "Mirror Across" "Plane" (Direction3d.mirrorAcross @Space @Meters) $(docs 'Direction3d.mirrorAcross)
    , member1 "Place In" "Basis" Direction3d.placeIn $(docs 'Direction3d.placeIn)
    , member1 "Relative To" "Basis" Direction3d.relativeTo $(docs 'Direction3d.relativeTo)
    , negateSelf
    ]

point3d :: Class
point3d =
  Class.new @(Point3d (Space @ Meters)) "A point in 3D." $
    [ constant "Origin" (Point3d.origin @Space @Meters) $(docs 'Point3d.origin)
    , factory2 "Along" "Axis" "Distance" Point3d.along $(docs 'Point3d.along)
    , factory2 "On" "Plane" "Position" Point3d.on $(docs 'Point3d.on)
    , factory2 "From Coordinates" "Convention" "Coordinates" Point3d.fromCoordinates $(docs 'Point3d.fromCoordinates)
    , member1 "Coordinates" "Convention" Point3d.coordinates $(docs 'Point3d.coordinates)
    , member1 "Distance To" "Other" Point3d.distanceFrom $(docs 'Point3d.distanceFrom)
    , member1 "Midpoint" "Other" Point3d.midpoint $(docs 'Point3d.midpoint)
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
    , factory1 "Hull" "Points" Bounds3d.hullN $(docs 'Bounds3d.hullN)
    , factory1 "Aggregate" "Bounds" Bounds3d.aggregateN $(docs 'Bounds3d.aggregateN)
    , member1 "Coordinates" "Convention" Bounds3d.coordinates $(docs 'Bounds3d.coordinates)
    , plus @(Vector3d (Space @ Meters)) Self
    , minus @(Vector3d (Space @ Meters)) Self
    ]
      <> affineTransformations3d Bounds3d.transformBy

axis3d :: Class
axis3d =
  Class.new @(Axis3d (Space @ Meters)) $(docs ''Axis3d) $
    [ factory1 "Rightward" "Origin Point" (Axis3d.rightward) $(docs 'Axis3d.rightward)
    , factory1 "Leftward" "Origin Point" (Axis3d.leftward) $(docs 'Axis3d.leftward)
    , factory1 "Forward" "Origin Point" (Axis3d.forward) $(docs 'Axis3d.forward)
    , factory1 "Backward" "Origin Point" (Axis3d.backward) $(docs 'Axis3d.backward)
    , factory1 "Upward" "Origin Point" (Axis3d.upward) $(docs 'Axis3d.upward)
    , factory1 "Downward" "Origin Point" (Axis3d.downward) $(docs 'Axis3d.downward)
    , member0 "Origin Point" Axis3d.originPoint $(docs 'Axis3d.originPoint)
    , member0 "Direction" Axis3d.direction $(docs 'Axis3d.direction)
    , member0 "Arbitrary Normal Plane" Axis3d.arbitraryNormalPlane $(docs 'Axis3d.arbitraryNormalPlane)
    , member1 "Move To" "Point" Axis3d.moveTo $(docs 'Axis3d.moveTo)
    , member0 "Reverse" Axis3d.reverse $(docs 'Axis3d.reverse)
    , member1 "Place In" "Frame" Axis3d.placeIn $(docs 'Axis3d.placeIn)
    , member1 "Relative To" "Frame" Axis3d.relativeTo $(docs 'Axis3d.relativeTo)
    ]
      <> orthonormalTransformations3d Axis3d.transformBy

planarBasis3d :: Class
planarBasis3d =
  Class.new @(PlanarBasis3d Space (Defines Space)) $(docs ''PlanarBasis3d) $
    [ constant "Forward Facing" (PlanarBasis3d.forwardFacing @Space @(Defines Space)) $(docs 'PlanarBasis3d.forwardFacing)
    , constant "Backward Facing" (PlanarBasis3d.backwardFacing @Space @(Defines Space)) $(docs 'PlanarBasis3d.backwardFacing)
    , constant "Rightward Facing" (PlanarBasis3d.rightwardFacing @Space @(Defines Space)) $(docs 'PlanarBasis3d.rightwardFacing)
    , constant "Leftward Facing" (PlanarBasis3d.leftwardFacing @Space @(Defines Space)) $(docs 'PlanarBasis3d.leftwardFacing)
    , constant "Upward Facing" (PlanarBasis3d.upwardFacing @Space @(Defines Space)) $(docs 'PlanarBasis3d.upwardFacing)
    , constant "Downward Facing" (PlanarBasis3d.downwardFacing @Space @(Defines Space)) $(docs 'PlanarBasis3d.downwardFacing)
    , member0 "X Direction" PlanarBasis3d.xDirection $(docs 'PlanarBasis3d.xDirection)
    , member0 "Y Direction" PlanarBasis3d.yDirection $(docs 'PlanarBasis3d.yDirection)
    , member0 "Normal Direction" PlanarBasis3d.normalDirection $(docs 'PlanarBasis3d.normalDirection)
    , member1 "Place In" "Basis" PlanarBasis3d.placeIn $(docs 'PlanarBasis3d.placeIn)
    , member1 "Relative To" "Basis" PlanarBasis3d.relativeTo $(docs 'PlanarBasis3d.relativeTo)
    ]

plane3d :: Class
plane3d =
  Class.new @(Plane3d (Space @ Meters) (Defines Space)) $(docs ''Plane3d) $
    [ constant "Top" (Plane3d.top @Space @Meters) $(docs 'Plane3d.top)
    , constant "Bottom" (Plane3d.bottom @Space @Meters) $(docs 'Plane3d.bottom)
    , constant "Front" (Plane3d.front @Space @Meters) $(docs 'Plane3d.front)
    , constant "Back" (Plane3d.back @Space @Meters) $(docs 'Plane3d.back)
    , constant "Left" (Plane3d.left @Space @Meters) $(docs 'Plane3d.left)
    , constant "Right" (Plane3d.right @Space @Meters) $(docs 'Plane3d.right)
    , factory1 "Forward Facing" "Origin Point" Plane3d.forwardFacing $(docs 'Plane3d.forwardFacing)
    , factory1 "Backward Facing" "Origin Point" Plane3d.backwardFacing $(docs 'Plane3d.backwardFacing)
    , factory1 "Leftward Facing" "Origin Point" Plane3d.leftwardFacing $(docs 'Plane3d.leftwardFacing)
    , factory1 "Rightward Facing" "Origin Point" Plane3d.rightwardFacing $(docs 'Plane3d.rightwardFacing)
    , factory1 "Upward Facing" "Origin Point" Plane3d.upwardFacing $(docs 'Plane3d.upwardFacing)
    , factory1 "Downward Facing" "Origin Point" Plane3d.downwardFacing $(docs 'Plane3d.downwardFacing)
    , factory2 "With Arbitrary Basis" "Origin Point" "Normal Direction" Plane3d.withArbitraryBasis $(docs 'Plane3d.withArbitraryBasis)
    , member0 "Origin Point" Plane3d.originPoint $(docs 'Plane3d.originPoint)
    , member0 "X Direction" Plane3d.xDirection $(docs 'Plane3d.xDirection)
    , member0 "Y Direction" Plane3d.yDirection $(docs 'Plane3d.yDirection)
    , member0 "Normal Direction" Plane3d.normalDirection $(docs 'Plane3d.normalDirection)
    , member0 "X Axis" Plane3d.xAxis $(docs 'Plane3d.xAxis)
    , member0 "Y Axis" Plane3d.yAxis $(docs 'Plane3d.yAxis)
    , member0 "Normal Axis" Plane3d.normalAxis $(docs 'Plane3d.normalAxis)
    , member0 "XN Plane" Plane3d.xnPlane $(docs 'Plane3d.xnPlane)
    , member0 "NX Plane" Plane3d.nxPlane $(docs 'Plane3d.nxPlane)
    , member0 "YN Plane" Plane3d.ynPlane $(docs 'Plane3d.ynPlane)
    , member0 "NY Plane" Plane3d.nyPlane $(docs 'Plane3d.nyPlane)
    , member1 "Move To" "Point" Plane3d.moveTo $(docs 'Plane3d.moveTo)
    , member0 "Flip X" Plane3d.flipX $(docs 'Plane3d.flipX)
    , member0 "Flip Y" Plane3d.flipY $(docs 'Plane3d.flipY)
    , member1 "Offset By" "Distance" Plane3d.offsetBy $(docs 'Plane3d.offsetBy)
    , member1 "Place In" "Frame" Plane3d.placeIn $(docs 'Plane3d.placeIn)
    , member1 "Relative To" "Frame" Plane3d.relativeTo $(docs 'Plane3d.relativeTo)
    ]
      <> rigidTransformations3d Plane3d.transformBy

basis3d :: Class
basis3d =
  Class.new @(Basis3d Space (Defines Space)) $(docs ''Basis3d) $
    [ constant "Identity" (Basis3d.identity @Space @(Defines Space)) $(docs 'Basis3d.identity)
    , constant "Forward Facing" (Basis3d.forwardFacing @Space @(Defines Space)) $(docs 'Basis3d.forwardFacing)
    , constant "Backward Facing" (Basis3d.backwardFacing @Space @(Defines Space)) $(docs 'Basis3d.backwardFacing)
    , constant "Rightward Facing" (Basis3d.rightwardFacing @Space @(Defines Space)) $(docs 'Basis3d.rightwardFacing)
    , constant "Leftward Facing" (Basis3d.leftwardFacing @Space @(Defines Space)) $(docs 'Basis3d.leftwardFacing)
    , constant "Upward Facing" (Basis3d.upwardFacing @Space @(Defines Space)) $(docs 'Basis3d.upwardFacing)
    , constant "Downward Facing" (Basis3d.downwardFacing @Space @(Defines Space)) $(docs 'Basis3d.downwardFacing)
    , member0 "Forward Direction" Basis3d.forwardDirection $(docs 'Basis3d.forwardDirection)
    , member0 "Backward Direction" Basis3d.backwardDirection $(docs 'Basis3d.backwardDirection)
    , member0 "Leftward Direction" Basis3d.leftwardDirection $(docs 'Basis3d.leftwardDirection)
    , member0 "Rightward Direction" Basis3d.rightwardDirection $(docs 'Basis3d.rightwardDirection)
    , member0 "Upward Direction" Basis3d.upwardDirection $(docs 'Basis3d.upwardDirection)
    , member0 "Downward Direction" Basis3d.downwardDirection $(docs 'Basis3d.downwardDirection)
    , member1 "Place In" "Other Basis" Basis3d.placeIn $(docs 'Basis3d.placeIn)
    , member1 "Relative To" "Other Basis" Basis3d.relativeTo $(docs 'Basis3d.relativeTo)
    ]

frame3d :: Class
frame3d =
  Class.new @(Frame3d (Space @ Meters) (Defines Space)) $(docs ''Frame3d) $
    [ constant "Identity" (Frame3d.identity @Space @Meters @(Defines Space)) $(docs 'Frame3d.identity)
    , factory1 "Forward Facing" "Origin Point" Frame3d.forwardFacing $(docs 'Frame3d.forwardFacing)
    , factory1 "Backward Facing" "Origin Point" Frame3d.backwardFacing $(docs 'Frame3d.backwardFacing)
    , factory1 "Rightward Facing" "Origin Point" Frame3d.rightwardFacing $(docs 'Frame3d.rightwardFacing)
    , factory1 "Leftward Facing" "Origin Point" Frame3d.leftwardFacing $(docs 'Frame3d.leftwardFacing)
    , factory1 "Upward Facing" "Origin Point" Frame3d.upwardFacing $(docs 'Frame3d.upwardFacing)
    , factory1 "Downward Facing" "Origin Point" Frame3d.downwardFacing $(docs 'Frame3d.downwardFacing)
    , factory1 "From Front Plane" "Plane" Frame3d.fromFrontPlane $(docs 'Frame3d.fromFrontPlane)
    , factory1 "From Back Plane" "Plane" Frame3d.fromBackPlane $(docs 'Frame3d.fromBackPlane)
    , factory1 "From Right Plane" "Plane" Frame3d.fromRightPlane $(docs 'Frame3d.fromRightPlane)
    , factory1 "From Left Plane" "Plane" Frame3d.fromLeftPlane $(docs 'Frame3d.fromLeftPlane)
    , factory1 "From Top Plane" "Plane" Frame3d.fromTopPlane $(docs 'Frame3d.fromTopPlane)
    , factory1 "From Bottom Plane" "Plane" Frame3d.fromBottomPlane $(docs 'Frame3d.fromBottomPlane)
    , factory2 "Align" "Local Frame" "Global Frame" Frame3d.align $(docs 'Frame3d.align)
    , factory2 "Mate" "Local Frame" "Global Frame" Frame3d.mate $(docs 'Frame3d.mate)
    , member0 "Origin Point" Frame3d.originPoint $(docs 'Frame3d.originPoint)
    , member0 "Forward Direction" Frame3d.forwardDirection $(docs 'Frame3d.forwardDirection)
    , member0 "Backward Direction" Frame3d.backwardDirection $(docs 'Frame3d.backwardDirection)
    , member0 "Rightward Direction" Frame3d.rightwardDirection $(docs 'Frame3d.rightwardDirection)
    , member0 "Leftward Direction" Frame3d.leftwardDirection $(docs 'Frame3d.leftwardDirection)
    , member0 "Upward Direction" Frame3d.upwardDirection $(docs 'Frame3d.upwardDirection)
    , member0 "Downward Direction" Frame3d.downwardDirection $(docs 'Frame3d.downwardDirection)
    , member0 "Forward Axis" Frame3d.forwardAxis $(docs 'Frame3d.forwardAxis)
    , member0 "Backward Axis" Frame3d.backwardAxis $(docs 'Frame3d.backwardAxis)
    , member0 "Rightward Axis" Frame3d.rightwardAxis $(docs 'Frame3d.rightwardAxis)
    , member0 "Leftward Axis" Frame3d.leftwardAxis $(docs 'Frame3d.leftwardAxis)
    , member0 "Upward Axis" Frame3d.upwardAxis $(docs 'Frame3d.upwardAxis)
    , member0 "Downward Axis" Frame3d.downwardAxis $(docs 'Frame3d.downwardAxis)
    , member0 "Front Plane" Frame3d.frontPlane $(docs 'Frame3d.frontPlane)
    , member0 "Back Plane" Frame3d.backPlane $(docs 'Frame3d.backPlane)
    , member0 "Right Plane" Frame3d.rightPlane $(docs 'Frame3d.rightPlane)
    , member0 "Left Plane" Frame3d.leftPlane $(docs 'Frame3d.leftPlane)
    , member0 "Top Plane" Frame3d.topPlane $(docs 'Frame3d.topPlane)
    , member0 "Bottom Plane" Frame3d.bottomPlane $(docs 'Frame3d.bottomPlane)
    , member1 "Place In" "Other Frame" Frame3d.placeIn $(docs 'Frame3d.placeIn)
    , member1 "Relative To" "Other Frame" Frame3d.relativeTo $(docs 'Frame3d.relativeTo)
    , member0 "Inverse" Frame3d.inverse $(docs 'Frame3d.inverse)
    , member0 "Reverse" Frame3d.reverse $(docs 'Frame3d.reverse)
    , member1 "Move To" "Point" Frame3d.moveTo $(docs 'Frame3d.moveTo)
    , member1 "Offset Forward By" "Distance" Frame3d.offsetForwardBy $(docs 'Frame3d.offsetForwardBy)
    , member1 "Offset Backward By" "Distance" Frame3d.offsetBackwardBy $(docs 'Frame3d.offsetBackwardBy)
    , member1 "Offset Rightward By" "Distance" Frame3d.offsetRightwardBy $(docs 'Frame3d.offsetRightwardBy)
    , member1 "Offset Leftward By" "Distance" Frame3d.offsetLeftwardBy $(docs 'Frame3d.offsetLeftwardBy)
    , member1 "Offset Upward By" "Distance" Frame3d.offsetUpwardBy $(docs 'Frame3d.offsetUpwardBy)
    , member1 "Offset Downward By" "Distance" Frame3d.offsetDownwardBy $(docs 'Frame3d.offsetDownwardBy)
    , member1 "Turn Right By" "Angle" Frame3d.turnRightBy $(docs 'Frame3d.turnRightBy)
    , member1 "Turn Left By" "Angle" Frame3d.turnLeftBy $(docs 'Frame3d.turnLeftBy)
    , member1 "Roll Right By" "Angle" Frame3d.rollRightBy $(docs 'Frame3d.rollRightBy)
    , member1 "Roll Left By" "Angle" Frame3d.rollLeftBy $(docs 'Frame3d.rollLeftBy)
    , member1 "Rotate Up By" "Angle" Frame3d.rotateUpBy $(docs 'Frame3d.rotateUpBy)
    , member1 "Rotate Down By" "Angle" Frame3d.rotateDownBy $(docs 'Frame3d.rotateDownBy)
    , member0 "Turn Right" Frame3d.turnRight $(docs 'Frame3d.turnRight)
    , member0 "Turn Left" Frame3d.turnLeft $(docs 'Frame3d.turnLeft)
    , member0 "Roll Right" Frame3d.rollRight $(docs 'Frame3d.rollRight)
    , member0 "Roll Left" Frame3d.rollLeft $(docs 'Frame3d.rollLeft)
    , member0 "Rotate Up" Frame3d.rotateUp $(docs 'Frame3d.rotateUp)
    , member0 "Rotate Down" Frame3d.rotateDown $(docs 'Frame3d.rotateDown)
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
    , factory4 "Polar Arc" "Center Point" "Radius" "Start Angle" "End Angle" Curve2d.polarArc $(docs 'Curve2d.polarArc)
    , factory3 "Swept Arc" "Center Point" "Start Point" "Swept Angle" Curve2d.sweptArc $(docs 'Curve2d.sweptArc)
    , factoryM4 "Corner Arc" "Corner Point" "Incoming Direction" "Outgoing Direction" "Radius" Curve2d.cornerArc $(docs 'Curve2d.cornerArc)
    , factory2 "Circle" "Center Point" "Diameter" Curve2d.circle $(docs 'Curve2d.circle)
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
      <> affineTransformations2d Curve2d.transformBy

uvCurve :: Class
uvCurve =
  Class.new @(Curve2d (Space @ Unitless)) $(docs ''Curve2d) $
    [ factory1 "Constant" "Point" Curve2d.constant $(docs 'Curve2d.constant)
    , factory2 "UV" "U Coordinate" "V Coordinate" Curve2d.xy $(docs 'Curve2d.xy)
    , factory2 "Line" "Start Point" "End Point" Curve2d.line $(docs 'Curve2d.line)
    , factoryU3 "Arc" "Start Point" "End Point" "Swept Angle" Curve2d.arc $(docs 'Curve2d.arc)
    , factory4 "Polar Arc" "Center Point" "Radius" "Start Angle" "End Angle" Curve2d.polarArc $(docs 'Curve2d.polarArc)
    , factory2 "Circle" "Center Point" "Diameter" Curve2d.circle $(docs 'Curve2d.circle)
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
      <> affineTransformations2d Curve2d.transformBy

region2d :: Class
region2d =
  Class.new @(Region2d (Space @ Meters)) $(docs ''Region2d) $
    [ factoryM1R "Bounded By" "Curves" Region2d.boundedBy $(docs 'Region2d.boundedBy)
    , factoryM1R "Rectangle" "Bounding Box" Region2d.rectangle $(docs 'Region2d.rectangle)
    , factoryM2R "Circle" "Center Point" "Diameter" Region2d.circle $(docs 'Region2d.circle)
    , factoryM1R "Polygon" "Points" Region2d.polygon $(docs 'Region2d.polygon)
    , member0 "Outer Loop" Region2d.outerLoop $(docs 'Region2d.outerLoop)
    , member0 "Inner Loops" Region2d.innerLoops $(docs 'Region2d.innerLoops)
    , member0 "Boundary Curves" Region2d.boundaryCurves $(docs 'Region2d.boundaryCurves)
    , memberM2 "Fillet" "Points" "Radius" Region2d.fillet $(docs 'Region2d.fillet)
    ]
      <> affineTransformations2d Region2d.transformBy

uvRegion :: Class
uvRegion =
  Class.new @(Region2d (Space @ Unitless)) $(docs ''Region2d) $
    [ constant "Unit Square" Region2d.unitSquare $(docs 'Region2d.unitSquare)
    , factoryU1R "Bounded By" "Curves" Region2d.boundedBy $(docs 'Region2d.boundedBy)
    , factoryU1R "Rectangle" "Bounding Box" Region2d.rectangle $(docs 'Region2d.rectangle)
    , factoryU2R "Circle" "Center Point" "Diameter" Region2d.circle $(docs 'Region2d.circle)
    , member0 "Outer Loop" Region2d.outerLoop $(docs 'Region2d.outerLoop)
    , member0 "Inner Loops" Region2d.innerLoops $(docs 'Region2d.innerLoops)
    , member0 "Boundary Curves" Region2d.boundaryCurves $(docs 'Region2d.boundaryCurves)
    ]
      <> affineTransformations2d Region2d.transformBy

body3d :: Class
body3d = do
  let writeStl path convention constraints body =
        Stl.writeBinary path convention Length.inMillimeters (Body3d.toMesh constraints body)
  Class.new @(Body3d (Space @ Meters)) $(docs ''Body3d) $
    [ factoryM3R "Extruded" "Sketch Plane" "Profile" "Distance" Body3d.extruded $(docs 'Body3d.extruded)
    , factoryM4R "Revolved" "Sketch Plane" "Profile" "Axis" "Angle" Body3d.revolved $(docs 'Body3d.revolved)
    , factoryM1R "Block" "Bounding Box" Body3d.block $(docs 'Body3d.block)
    , factoryM2R "Sphere" "Center Point" "Diameter" Body3d.sphere $(docs 'Body3d.sphere)
    , factoryM3R "Cylinder" "Start Point" "End Point" "Diameter" Body3d.cylinder $(docs 'Body3d.cylinder)
    , factoryM3R "Cylinder Along" "Axis" "Distance" "Diameter" Body3d.cylinderAlong $(docs 'Body3d.cylinderAlong)
    , memberM3 "Write STL" "Path" "Convention" "Mesh Constraints" writeStl "Write a body to a binary STL file, using units of millimeters."
    ]

mesh :: Class
mesh =
  Class.static "Mesh" "Meshing-related functionality." $
    [ nested @(Mesh.Constraint Meters) $(docs ''Mesh.Constraint) []
    , static1 "Max Error" "Error" (Mesh.maxError @Meters) $(docs 'Mesh.maxError)
    , static1 "Max Size" "Size" (Mesh.maxSize @Meters) $(docs 'Mesh.maxSize)
    ]

scene3d :: Class
scene3d =
  Class.static "Scene3d" "A set of functions for constructing 3D scenes." $
    [ staticM3 "Body" "Mesh Constraints" "Material" "Body" (Scene3d.body @Space) $(docs 'Scene3d.body)
    , static1 "Group" "Entities" (Scene3d.group @Space) $(docs 'Scene3d.group)
    , static2 "Metal" "Base Color" "Roughness" Scene3d.metal $(docs 'Scene3d.metal)
    , static1 "Aluminum" "Roughness" Scene3d.aluminum $(docs 'Scene3d.aluminum)
    , static1 "Brass" "Roughness" Scene3d.brass $(docs 'Scene3d.brass)
    , static1 "Chromium" "Roughness" Scene3d.chromium $(docs 'Scene3d.chromium)
    , static1 "Copper" "Roughness" Scene3d.copper $(docs 'Scene3d.copper)
    , static1 "Gold" "Roughness" Scene3d.gold $(docs 'Scene3d.gold)
    , static1 "Iron" "Roughness" Scene3d.iron $(docs 'Scene3d.iron)
    , static1 "Nickel" "Roughness" Scene3d.nickel $(docs 'Scene3d.nickel)
    , static1 "Silver" "Roughness" Scene3d.silver $(docs 'Scene3d.silver)
    , static1 "Titanium" "Roughness" Scene3d.titanium $(docs 'Scene3d.titanium)
    , static2 "Nonmetal" "Base Color" "Roughness" Scene3d.nonmetal $(docs 'Scene3d.nonmetal)
    , static3 "Material" "Base Color" "Metallic" "Roughness" Scene3d.material $(docs 'Scene3d.material)
    , static2 "Write GLB" "Path" "Entities" (Scene3d.writeGlb @Space) $(docs 'Scene3d.writeGlb)
    , nested @(Scene3d.Entity Space) "A scene entity such as a mesh or group." (rigidTransformations3d Scene3d.transformBy)
    , nested @Scene3d.Material "A material applied to a mesh." []
    ]

spurGear :: Class
spurGear =
  Class.new @SpurGear $(docs ''SpurGear) $
    [ factory2 "Metric" "Num Teeth" "Module" SpurGear.metric $(docs 'SpurGear.metric)
    , member0 "Num Teeth" SpurGear.numTeeth $(docs 'SpurGear.numTeeth)
    , member0 "Module" SpurGear.module_ $(docs 'SpurGear.module_)
    , member0 "Pitch Diameter" SpurGear.pitchDiameter $(docs 'SpurGear.pitchDiameter)
    , member0 "Outer Diameter" SpurGear.outerDiameter $(docs 'SpurGear.outerDiameter)
    , memberM0 "Profile" SpurGear.profile $(docs 'SpurGear.profile)
    ]
