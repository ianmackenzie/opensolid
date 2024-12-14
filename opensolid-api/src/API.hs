module API (Class (..), Function (..), classes, functions) where

import API.AbsFunction qualified as AbsFunction
import API.BinaryOperator qualified as BinaryOperator
import API.ComparisonFunction qualified as ComparisonFunction
import API.Constant (Constant (Constant))
import API.Constant qualified as Constant
import API.Constraint (Constraint)
import API.Docs (docs)
import API.EqualityFunction qualified as EqualityFunction
import API.MemberFunction (MemberFunction (..))
import API.MemberFunction qualified as MemberFunction
import API.NegationFunction qualified as NegationFunction
import API.PostOperator (PostOperator (PostOperator))
import API.PostOperator qualified as PostOperator
import API.PreOperator (PreOperator (PreOperator))
import API.PreOperator qualified as PreOperator
import API.StaticFunction (StaticFunction (..))
import API.StaticFunction qualified as StaticFunction
import Angle (Angle)
import Angle qualified
import Area (Area)
import Area qualified
import Bounds2d (Bounds2d)
import Bounds2d qualified
import Color (Color)
import Color qualified
import Curve1d (Curve1d)
import Curve1d qualified
import Curve1d.Zero qualified
import Data.Proxy (Proxy (Proxy))
import Direction2d (Direction2d)
import Direction2d qualified
import Drawing2d qualified
import Foreign (Ptr)
import Length (Length)
import Length qualified
import List qualified
import OpenSolid
import OpenSolid.FFI (FFI, Name)
import OpenSolid.FFI qualified as FFI
import Pair qualified
import Point2d (Point2d)
import Point2d qualified
import Qty qualified
import Range (Range)
import Range qualified
import Units (Meters, Radians, SquareMeters)
import Vector2d (Vector2d)
import Vector2d qualified

data Space

data Class where
  Class ::
    FFI value =>
    { id :: FFI.Id value
    , documentation :: Text
    , constants :: List (Name, Constant)
    , staticFunctions :: List (Name, StaticFunction)
    , memberFunctions :: List (Name, MemberFunction value)
    , equalityFunction :: Maybe (value -> value -> Bool)
    , comparisonFunction :: Maybe (value -> value -> Int)
    , negationFunction :: Maybe (value -> value)
    , absFunction :: Maybe (value -> value)
    , preOperators :: List (BinaryOperator.Id, List (PreOperator value))
    , postOperators :: List (BinaryOperator.Id, List (PostOperator value))
    , nestedClasses :: List Class
    } ->
    Class

data Function = Function
  { ffiName :: Text
  , constraint :: Maybe Constraint
  , argumentTypes :: List FFI.Type
  , returnType :: FFI.Type
  , invoke :: Ptr () -> Ptr () -> IO ()
  }

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
  ]

length :: Class
length =
  class_ @Length
    $(docs ''Length)
    [ constant "Zero" Length.zero $(docs 'Length.zero)
    , constant "Meter" Length.meter $(docs 'Length.meter)
    , constant "Centimeter" Length.centimeter $(docs 'Length.centimeter)
    , constant "Millimeter" Length.millimeter $(docs 'Length.millimeter)
    , constant "Inch" Length.inch $(docs 'Length.inch)
    , constant "Pixel" Length.pixel $(docs 'Length.pixel)
    , factory1 "Meters" "Value" Length.meters $(docs 'Length.meters)
    , factory1 "Centimeters" "Value" Length.centimeters $(docs 'Length.centimeters)
    , factory1 "Millimeters" "Value" Length.millimeters $(docs 'Length.millimeters)
    , factory1 "Inches" "Value" Length.inches $(docs 'Length.inches)
    , factory1 "Pixels" "Value" Length.pixels $(docs 'Length.pixels)
    , member0 "In Meters" Length.inMeters $(docs 'Length.inMeters)
    , member0 "In Centimeters" Length.inCentimeters $(docs 'Length.inCentimeters)
    , member0 "In Millimeters" Length.inMillimeters $(docs 'Length.inMillimeters)
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
    , plus @(Curve1d Meters) Self
    , minusSelf
    , minus @(Range Meters) Self
    , minus @(Curve1d Meters) Self
    , timesFloat
    , timesSelf
    , times @(Range Unitless) Self
    , times @(Range Meters) Self
    , times @(Curve1d Unitless) Self
    , times @(Curve1d Meters) Self
    , times @(Direction2d Space) Self
    , times @(Vector2d (Space @ Unitless)) Self
    , times @(Vector2d (Space @ Meters)) Self
    , divByFloat
    , divBySelf
    , divBy @(Range Unitless) Self
    , divBy @(Range Meters) Self
    , divBy @(Curve1d Unitless) Self
    , divBy @(Curve1d Meters) Self
    , floorDivBySelf
    , modBySelf
    ]

area :: Class
area =
  class_ @Area
    $(docs ''Area)
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
    , plus @(Curve1d SquareMeters) Self
    , minusSelf
    , minus @(Range SquareMeters) Self
    , minus @(Curve1d SquareMeters) Self
    , timesFloat
    , times @(Range Unitless) Self
    , times @(Curve1d Unitless) Self
    , times @(Direction2d Space) Self
    , times @(Vector2d (Space @ Unitless)) Self
    , divByFloat
    , divBySelf
    , divBy @Length Self
    , divBy @(Range Unitless) Self
    , divBy @(Range Meters) Self
    , divBy @(Range SquareMeters) Self
    , divBy @(Curve1d Unitless) Self
    , divBy @(Curve1d Meters) Self
    , divBy @(Curve1d SquareMeters) Self
    , floorDivBySelf
    , modBySelf
    ]

angle :: Class
angle =
  class_ @Angle
    $(docs ''Angle)
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
    , plus @(Curve1d Radians) Self
    , minusSelf
    , minus @(Range Radians) Self
    , minus @(Curve1d Radians) Self
    , timesFloat
    , times @(Range Unitless) Self
    , times @(Curve1d Unitless) Self
    , divByFloat
    , divBySelf
    , divBy @(Range Unitless) Self
    , divBy @(Range Radians) Self
    , divBy @(Curve1d Unitless) Self
    , divBy @(Curve1d Radians) Self
    , floorDivBySelf
    , modBySelf
    ]

range :: Class
range =
  class_ @(Range Unitless)
    "A range of unitless values, with a lower bound and upper bound."
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
    , times @Angle Self
    , times @(Range Meters) Self
    , times @(Range SquareMeters) Self
    , divByFloat
    , divBySelf
    ]

lengthRange :: Class
lengthRange =
  class_ @(Range Meters)
    "A range of length values, with a lower bound and upper bound."
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
  class_ @(Range SquareMeters)
    "A range of area values, with a lower bound and upper bound."
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
  class_ @(Range Radians)
    "A range of angle values, with a lower bound and upper bound."
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
    , divByFloat
    , divBySelf
    , divBy @(Range Unitless) Self
    ]

color :: Class
color =
  class_ @Color
    $(docs ''Color)
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
  class_ @(Vector2d (Space @ Unitless))
    "A unitless vector in 2D."
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
  class_ @(Vector2d (Space @ Meters))
    "A displacement vector in 2D."
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
  class_ @(Vector2d (Space @ SquareMeters))
    "A vector in 2D with units of area."
    [ constant "Zero" (Vector2d.zero @Space @SquareMeters) $(docs 'Vector2d.zero)
    , factory2 "XY" "X Component" "Y Component" Vector2d.xy $(docs 'Vector2d.xy)
    , factory1 "X" "X Component" Vector2d.x $(docs 'Vector2d.x)
    , factory1 "Y" "Y Component" Vector2d.y $(docs 'Vector2d.y)
    , factory2 "Polar" "Magnitude" "Angle" Vector2d.polar $(docs 'Vector2d.polar)
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
  class_ @(Direction2d Space)
    $(docs ''Direction2d)
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
  class_ @(Point2d (Space @ Meters))
    "A point in 2D, defined by its X and Y coordinates."
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
    ]

uvPoint :: Class
uvPoint =
  class_ @(Point2d (Space @ Unitless))
    "A point in UV parameter space."
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
  class_ @(Bounds2d (Space @ Meters))
    "A bounding box in 2D."
    [ factory2 "XY" "X Coordinate" "Y Coordinate" Bounds2d.xy $(docs 'Bounds2d.xy)
    , factory1 "Constant" "Point" Bounds2d.constant $(docs 'Bounds2d.constant)
    , factory2 "From Corners" "P1" "P2" Bounds2d.hull2 $(docs 'Bounds2d.hull2)
    , factory1 "Hull" "Points" Bounds2d.hullN $(docs 'Bounds2d.hullN)
    , factory1 "Aggregate" "Bounds" Bounds2d.aggregateN $(docs 'Bounds2d.aggregateN)
    , member0 "Coordinates" Bounds2d.coordinates $(docs 'Bounds2d.coordinates)
    , member0 "X Coordinate" Bounds2d.xCoordinate $(docs 'Bounds2d.xCoordinate)
    , member0 "Y Coordinate" Bounds2d.yCoordinate $(docs 'Bounds2d.yCoordinate)
    ]

uvBounds :: Class
uvBounds =
  class_ @(Bounds2d (Space @ Unitless))
    "A bounding box in UV parameter space."
    [ factory2 "UV" "U Coordinate" "V Coordinate" Bounds2d.xy $(docs 'Bounds2d.xy)
    , factory1 "Constant" "Point" Bounds2d.constant $(docs 'Bounds2d.constant)
    , factory2 "From Corners" "P1" "P2" Bounds2d.hull2 $(docs 'Bounds2d.hull2)
    , factory1 "Hull" "Points" Bounds2d.hullN $(docs 'Bounds2d.hullN)
    , factory1 "Aggregate" "Bounds" Bounds2d.aggregateN $(docs 'Bounds2d.aggregateN)
    , member0 "Coordinates" Bounds2d.coordinates $(docs 'Bounds2d.coordinates)
    , member0 "U Coordinate" Bounds2d.xCoordinate $(docs 'Bounds2d.xCoordinate)
    , member0 "V Coordinate" Bounds2d.yCoordinate $(docs 'Bounds2d.yCoordinate)
    ]

curve :: Class
curve =
  class_ @(Curve1d Unitless)
    "A parametric curve definining a unitless value in terms of a parameter value."
    [ constant "Zero" (Curve1d.zero @Unitless) $(docs 'Curve1d.zero)
    , constant "T" Curve1d.t $(docs 'Curve1d.t)
    , factory1 "Constant" "Value" Curve1d.constant $(docs 'Curve1d.constant)
    , member0 "Squared" Curve1d.squared $(docs 'Curve1d.squared)
    , member0 "Sqrt" Curve1d.sqrt $(docs 'Curve1d.sqrt)
    , member1 "Evaluate" "Parameter Value" (\t c -> Curve1d.evaluate c t) $(docs 'Curve1d.evaluate)
    , memberU0 "Zeros" Curve1d.zeros $(docs 'Curve1d.zeros)
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
    , times @(Curve1d Meters) Self
    , times @(Curve1d Radians) Self
    , divByFloat
    , divBySelf
    , nested @Curve1d.Zero
        "A point where a given curve is equal to zero."
        [ member0 "Location" Curve1d.Zero.location $(docs 'Curve1d.Zero.location)
        , member0 "Order" Curve1d.Zero.order $(docs 'Curve1d.Zero.order)
        , member0 "Sign" ((1 *) . Curve1d.Zero.sign) $(docs 'Curve1d.Zero.sign)
        ]
    ]

angleCurve :: Class
angleCurve =
  class_ @(Curve1d Radians)
    "A parametric curve definining an angle in terms of a parameter value."
    [ constant "Zero" (Curve1d.zero @Radians) $(docs 'Curve1d.zero)
    , factory1 "Constant" "Value" Curve1d.constant $(docs 'Curve1d.constant)
    , member0 "Sin" Curve1d.sin $(docs 'Curve1d.sin)
    , member0 "Cos" Curve1d.cos $(docs 'Curve1d.cos)
    , member1 "Evaluate" "Parameter Value" (\t c -> Curve1d.evaluate c t) $(docs 'Curve1d.evaluate)
    , memberR0 "Zeros" Curve1d.zeros $(docs 'Curve1d.zeros)
    , memberR0 "Is Zero" (~= Angle.zero) "Check if a curve is zero everywhere, within the current tolerance."
    , negateSelf
    , floatTimes
    , plusSelf
    , plus @Angle Self
    , minusSelf
    , minus @Angle Self
    , timesFloat
    , times @(Curve1d Unitless) Self
    , divByFloat
    , divBySelf
    , divBy @Angle Self
    , divBy @(Curve1d Unitless) Self
    ]

lengthCurve :: Class
lengthCurve =
  class_ @(Curve1d Meters)
    "A parametric curve definining a length in terms of a parameter value."
    [ constant "Zero" (Curve1d.zero @Meters) $(docs 'Curve1d.zero)
    , factory1 "Constant" "Value" Curve1d.constant $(docs 'Curve1d.constant)
    , member1 "Evaluate" "Parameter Value" (\t c -> Curve1d.evaluate c t) $(docs 'Curve1d.evaluate)
    , memberM0 "Zeros" Curve1d.zeros $(docs 'Curve1d.zeros)
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
    , times @(Curve1d Unitless) Self
    , divByFloat
    , divBySelf
    , divBy @Length Self
    , divBy @(Curve1d Unitless) Self
    ]

areaCurve :: Class
areaCurve =
  class_ @(Curve1d SquareMeters)
    "A parametric curve definining an area in terms of a parameter value."
    [ constant "Zero" (Curve1d.zero @SquareMeters) $(docs 'Curve1d.zero)
    , factory1 "Constant" "Value" Curve1d.constant $(docs 'Curve1d.constant)
    , member1 "Evaluate" "Parameter Value" (\t c -> Curve1d.evaluate c t) $(docs 'Curve1d.evaluate)
    , memberSM0 "Zeros" Curve1d.zeros $(docs 'Curve1d.zeros)
    , memberSM0 "Is Zero" (~= Area.zero) "Check if a curve is zero everywhere, within the current tolerance."
    , negateSelf
    , floatTimes
    , plusSelf
    , minusSelf
    , timesFloat
    , times @(Curve1d Unitless) Self
    , divByFloat
    , divBySelf
    , divBy @Length Self
    , divBy @Area Self
    , divBy @(Curve1d Unitless) Self
    , divBy @(Curve1d Meters) Self
    ]

data Drawing2d_

instance FFI Drawing2d_ where
  representation = FFI.classRepresentation "Drawing2d"

drawing2d :: Class
drawing2d =
  class_ @Drawing2d_
    "A set of functions for constructing 2D drawings."
    [ static2 "To SVG" "View Box" "Entities" Drawing2d.toSvg $(docs 'Drawing2d.toSvg)
    , static2 "Polygon" "Attributes" "Vertices" Drawing2d.polygon $(docs 'Drawing2d.polygon)
    , static3 "Circle" "Attributes" "Center Point" "Radius" Drawing2d.circle $(docs 'Drawing2d.circle)
    , constant "Black Stroke" Drawing2d.blackStroke $(docs 'Drawing2d.blackStroke)
    , static1 "Stroke Color" "Color" Drawing2d.strokeColor $(docs 'Drawing2d.strokeColor)
    , constant "No Fill" Drawing2d.noFill $(docs 'Drawing2d.noFill)
    , static1 "Fill Color" "Color" Drawing2d.fillColor $(docs 'Drawing2d.fillColor)
    , nested @(Drawing2d.Entity Space) "A drawing entity such as a shape or group." []
    , nested @(Drawing2d.Attribute Space) "A drawing attribute such as fill color or stroke width." []
    ]

----- CLASS MEMBERS -----

class_ :: forall value. FFI value => Text -> List (Member value) -> Class
class_ classDocs members = buildClass classDocs members [] [] [] Nothing Nothing Nothing Nothing [] [] []

data Member value where
  Const :: FFI result => Text -> result -> Text -> Member value
  Static1 :: (FFI a, FFI result) => Text -> Text -> (a -> result) -> Text -> Member value
  Static2 :: (FFI a, FFI b, FFI result) => Text -> Text -> Text -> (a -> b -> result) -> Text -> Member value
  Static3 :: (FFI a, FFI b, FFI c, FFI result) => Text -> Text -> Text -> Text -> (a -> b -> c -> result) -> Text -> Member value
  Static4 :: (FFI a, FFI b, FFI c, FFI d, FFI result) => Text -> Text -> Text -> Text -> Text -> (a -> b -> c -> d -> result) -> Text -> Member value
  Member0 :: (FFI value, FFI result) => Text -> (value -> result) -> Text -> Member value
  MemberU0 :: (FFI value, FFI result) => Text -> (Tolerance Unitless => value -> result) -> Text -> Member value
  MemberR0 :: (FFI value, FFI result) => Text -> (Tolerance Radians => value -> result) -> Text -> Member value
  MemberM0 :: (FFI value, FFI result) => Text -> (Tolerance Meters => value -> result) -> Text -> Member value
  MemberSM0 :: (FFI value, FFI result) => Text -> (Tolerance SquareMeters => value -> result) -> Text -> Member value
  Member1 :: (FFI a, FFI value, FFI result) => Text -> Text -> (a -> value -> result) -> Text -> Member value
  Equality :: Eq value => Member value
  Comparison :: Ord value => Member value
  Negate :: Negation value => Member value
  Abs :: (value -> value) -> Member value
  PreOp :: (FFI lhs, FFI result) => BinaryOperator.Id -> (lhs -> value -> result) -> Member value
  PostOp :: (FFI rhs, FFI result) => BinaryOperator.Id -> (value -> rhs -> result) -> Member value
  Nested :: FFI nested => Text -> List (Member nested) -> Member value

constant :: FFI result => Text -> result -> Text -> Member value
constant = Const

factory1 :: (FFI a, FFI value) => Text -> Text -> (a -> value) -> Text -> Member value
factory1 = Static1

factory2 :: (FFI a, FFI b, FFI value) => Text -> Text -> Text -> (a -> b -> value) -> Text -> Member value
factory2 = Static2

factory3 :: (FFI a, FFI b, FFI c, FFI value) => Text -> Text -> Text -> Text -> (a -> b -> c -> value) -> Text -> Member value
factory3 = Static3

static1 :: (FFI a, FFI result) => Text -> Text -> (a -> result) -> Text -> Member value
static1 = Static1

static2 :: (FFI a, FFI b, FFI result) => Text -> Text -> Text -> (a -> b -> result) -> Text -> Member value
static2 = Static2

static3 :: (FFI a, FFI b, FFI c, FFI result) => Text -> Text -> Text -> Text -> (a -> b -> c -> result) -> Text -> Member value
static3 = Static3

member0 :: (FFI value, FFI result) => Text -> (value -> result) -> Text -> Member value
member0 = Member0

memberU0 :: (FFI value, FFI result) => Text -> (Tolerance Unitless => value -> result) -> Text -> Member value
memberU0 = MemberU0

memberR0 :: (FFI value, FFI result) => Text -> (Tolerance Radians => value -> result) -> Text -> Member value
memberR0 = MemberR0

memberM0 :: (FFI value, FFI result) => Text -> (Tolerance Meters => value -> result) -> Text -> Member value
memberM0 = MemberM0

memberSM0 :: (FFI value, FFI result) => Text -> (Tolerance SquareMeters => value -> result) -> Text -> Member value
memberSM0 = MemberSM0

member1 :: (FFI a, FFI value, FFI result) => Text -> Text -> (a -> value -> result) -> Text -> Member value
member1 = Member1

data Self a = Self

equality :: Eq value => Member value
equality = Equality

comparison :: Ord value => Member value
comparison = Comparison

comparisonImpl :: Ord a => a -> a -> Int
comparisonImpl lhs rhs = case compare lhs rhs of
  LT -> -1
  EQ -> 0
  GT -> 1

negateSelf :: Negation value => Member value
negateSelf = Negate

absSelf :: (value -> value) -> Member value
absSelf = Abs

floatPlus ::
  forall value result.
  (Addition Float value result, FFI value, FFI result) =>
  Member value
floatPlus = PreOp BinaryOperator.Add ((+) :: Float -> value -> result)

floatMinus ::
  forall value result.
  (Subtraction Float value result, FFI value, FFI result) =>
  Member value
floatMinus = PreOp BinaryOperator.Sub ((-) :: Float -> value -> result)

floatTimes ::
  forall value result.
  (Multiplication Float value result, FFI value, FFI result) =>
  Member value
floatTimes = PreOp BinaryOperator.Mul ((*) :: Float -> value -> result)

floatDivBy ::
  forall value result.
  (Division Float value result, FFI value, FFI result) =>
  Member value
floatDivBy = PreOp BinaryOperator.Div ((/) :: Float -> value -> result)

plus ::
  forall rhs value result.
  (Addition value rhs result, FFI value, FFI rhs, FFI result) =>
  Self (value -> rhs -> result) ->
  Member value
plus _ = PostOp BinaryOperator.Add ((+) :: value -> rhs -> result)

plusFloat ::
  forall value result.
  (Addition value Float result, FFI value, FFI result) =>
  Member value
plusFloat = plus @Float Self

plusSelf ::
  forall value result.
  (Addition value value result, FFI value, FFI result) =>
  Member value
plusSelf = plus @value Self

minus ::
  forall rhs value result.
  (Subtraction value rhs result, FFI value, FFI rhs, FFI result) =>
  Self (value -> rhs -> result) ->
  Member value
minus _ = PostOp BinaryOperator.Sub ((-) :: value -> rhs -> result)

minusFloat ::
  forall value result.
  (Subtraction value Float result, FFI value, FFI result) =>
  Member value
minusFloat = minus @Float Self

minusSelf ::
  forall value result.
  (Subtraction value value result, FFI value, FFI result) =>
  Member value
minusSelf = minus @value Self

times ::
  forall rhs value result.
  (Multiplication value rhs result, FFI value, FFI rhs, FFI result) =>
  Self (value -> rhs -> result) ->
  Member value
times _ = PostOp BinaryOperator.Mul ((*) :: value -> rhs -> result)

timesFloat ::
  forall value result.
  (Multiplication value Float result, FFI value, FFI result) =>
  Member value
timesFloat = times @Float Self

timesSelf ::
  forall value result.
  (Multiplication value value result, FFI value, FFI result) =>
  Member value
timesSelf = times @value Self

divBy ::
  forall rhs value result.
  (Division value rhs result, FFI value, FFI rhs, FFI result) =>
  Self (value -> rhs -> result) ->
  Member value
divBy _ = PostOp BinaryOperator.Div ((/) :: value -> rhs -> result)

divByFloat ::
  forall value result.
  (Division value Float result, FFI value, FFI result) =>
  Member value
divByFloat = divBy @Float Self

divBySelf ::
  forall value result.
  (Division value value result, FFI value, FFI result) =>
  Member value
divBySelf = divBy @value Self

floorDivBySelf :: forall value. (DivMod value, FFI value) => Member value
floorDivBySelf = PostOp BinaryOperator.FloorDiv ((//) :: value -> value -> Int)

modBySelf :: forall value. (DivMod value, FFI value) => Member value
modBySelf = PostOp BinaryOperator.Mod ((%) :: value -> value -> value)

dot ::
  forall rhs value result.
  (DotMultiplication value rhs result, FFI value, FFI rhs, FFI result) =>
  Self (value -> rhs -> result) ->
  Member value
dot _ = PostOp BinaryOperator.Dot ((<>) :: value -> rhs -> result)

dotSelf ::
  forall value result.
  (DotMultiplication value value result, FFI value, FFI result) =>
  Member value
dotSelf = dot @value Self

cross ::
  forall rhs value result.
  (CrossMultiplication value rhs result, FFI value, FFI rhs, FFI result) =>
  Self (value -> rhs -> result) ->
  Member value
cross _ = PostOp BinaryOperator.Cross ((><) :: value -> rhs -> result)

crossSelf ::
  forall value result.
  (CrossMultiplication value value result, FFI value, FFI result) =>
  Member value
crossSelf = cross @value Self

nested :: FFI nested => Text -> List (Member nested) -> Member value
nested = Nested

addPreOverload ::
  BinaryOperator.Id ->
  PreOperator value ->
  List (BinaryOperator.Id, List (PreOperator value)) ->
  List (BinaryOperator.Id, List (PreOperator value))
addPreOverload operatorId overload [] = [(operatorId, [overload])]
addPreOverload operatorId overload (first : rest) = do
  let (existingId, existingOverloads) = first
  if operatorId == existingId
    then (existingId, existingOverloads + [overload]) : rest
    else first : addPreOverload operatorId overload rest

addPostOverload ::
  BinaryOperator.Id ->
  PostOperator value ->
  List (BinaryOperator.Id, List (PostOperator value)) ->
  List (BinaryOperator.Id, List (PostOperator value))
addPostOverload operatorId overload [] = [(operatorId, [overload])]
addPostOverload operatorId overload (first : rest) = do
  let (existingId, existingOverloads) = first
  if operatorId == existingId
    then (existingId, existingOverloads + [overload]) : rest
    else first : addPostOverload operatorId overload rest

buildClass ::
  forall value.
  FFI value =>
  Text ->
  List (Member value) ->
  List (Name, Constant) ->
  List (Name, StaticFunction) ->
  List (Name, MemberFunction value) ->
  Maybe (value -> value -> Bool) ->
  Maybe (value -> value -> Int) ->
  Maybe (value -> value) ->
  Maybe (value -> value) ->
  List (BinaryOperator.Id, List (PreOperator value)) ->
  List (BinaryOperator.Id, List (PostOperator value)) ->
  List Class ->
  Class
buildClass
  classDocs
  members
  constantsAcc
  staticFunctionsAcc
  memberFunctionsAcc
  equalityFunctionAcc
  comparisonFunctionAcc
  negationFunctionAcc
  absFunctionAcc
  preOperatorsAcc
  postOperatorsAcc
  nestedClassesAcc =
    case members of
      [] ->
        Class
          { id = case FFI.typeOf @value Proxy of
              FFI.Class (FFI.Id Proxy names) -> FFI.Id Proxy names
              _ -> internalError "Every class defined in the API must correspond to an FFI type with class representation"
          , documentation = classDocs
          , constants = constantsAcc
          , staticFunctions = staticFunctionsAcc
          , memberFunctions = memberFunctionsAcc
          , equalityFunction = equalityFunctionAcc
          , comparisonFunction = comparisonFunctionAcc
          , negationFunction = negationFunctionAcc
          , absFunction = absFunctionAcc
          , preOperators = preOperatorsAcc
          , postOperators = postOperatorsAcc
          , nestedClasses = nestedClassesAcc
          }
      first : rest -> do
        let addStatic name staticFunction =
              buildClass
                classDocs
                rest
                constantsAcc
                (staticFunctionsAcc + [(FFI.name name, staticFunction)])
                memberFunctionsAcc
                equalityFunctionAcc
                comparisonFunctionAcc
                negationFunctionAcc
                absFunctionAcc
                preOperatorsAcc
                postOperatorsAcc
                nestedClassesAcc
        let addMember name memberFunction =
              buildClass
                classDocs
                rest
                constantsAcc
                staticFunctionsAcc
                (memberFunctionsAcc + [(FFI.name name, memberFunction)])
                equalityFunctionAcc
                comparisonFunctionAcc
                negationFunctionAcc
                absFunctionAcc
                preOperatorsAcc
                postOperatorsAcc
                nestedClassesAcc
        case first of
          Const name value documentation ->
            buildClass
              classDocs
              rest
              (constantsAcc + [(FFI.name name, Constant value documentation)])
              staticFunctionsAcc
              memberFunctionsAcc
              equalityFunctionAcc
              comparisonFunctionAcc
              negationFunctionAcc
              absFunctionAcc
              preOperatorsAcc
              postOperatorsAcc
              nestedClassesAcc
          Static1 name arg1 f staticDocs ->
            addStatic name (StaticFunction1 (FFI.name arg1) f staticDocs)
          Static2 name arg1 arg2 f staticDocs ->
            addStatic name (StaticFunction2 (FFI.name arg1) (FFI.name arg2) f staticDocs)
          Static3 name arg1 arg2 arg3 f staticDocs ->
            addStatic name (StaticFunction3 (FFI.name arg1) (FFI.name arg2) (FFI.name arg3) f staticDocs)
          Static4 name arg1 arg2 arg3 arg4 f staticDocs ->
            addStatic name (StaticFunction4 (FFI.name arg1) (FFI.name arg2) (FFI.name arg3) (FFI.name arg4) f staticDocs)
          Member0 name f memberDocs ->
            addMember name (MemberFunction0 f memberDocs)
          MemberU0 name f memberDocs ->
            addMember name (MemberFunctionU0 f memberDocs)
          MemberR0 name f memberDocs ->
            addMember name (MemberFunctionR0 f memberDocs)
          MemberM0 name f memberDocs ->
            addMember name (MemberFunctionM0 f memberDocs)
          MemberSM0 name f memberDocs ->
            addMember name (MemberFunctionSM0 f memberDocs)
          Member1 name arg1 f memberDocs ->
            addMember name (MemberFunction1 (FFI.name arg1) f memberDocs)
          Equality ->
            buildClass
              classDocs
              rest
              constantsAcc
              staticFunctionsAcc
              memberFunctionsAcc
              (Just (==))
              comparisonFunctionAcc
              negationFunctionAcc
              absFunctionAcc
              preOperatorsAcc
              postOperatorsAcc
              nestedClassesAcc
          Comparison ->
            buildClass
              classDocs
              rest
              constantsAcc
              staticFunctionsAcc
              memberFunctionsAcc
              equalityFunctionAcc
              (Just comparisonImpl)
              negationFunctionAcc
              absFunctionAcc
              preOperatorsAcc
              postOperatorsAcc
              nestedClassesAcc
          Negate ->
            buildClass
              classDocs
              rest
              constantsAcc
              staticFunctionsAcc
              memberFunctionsAcc
              equalityFunctionAcc
              comparisonFunctionAcc
              (Just negate)
              absFunctionAcc
              preOperatorsAcc
              postOperatorsAcc
              nestedClassesAcc
          Abs function ->
            buildClass
              classDocs
              rest
              constantsAcc
              staticFunctionsAcc
              memberFunctionsAcc
              equalityFunctionAcc
              comparisonFunctionAcc
              negationFunctionAcc
              (Just function)
              preOperatorsAcc
              postOperatorsAcc
              nestedClassesAcc
          PreOp operatorId operator ->
            buildClass
              classDocs
              rest
              constantsAcc
              staticFunctionsAcc
              memberFunctionsAcc
              equalityFunctionAcc
              comparisonFunctionAcc
              negationFunctionAcc
              absFunctionAcc
              (addPreOverload operatorId (PreOperator operator) preOperatorsAcc)
              postOperatorsAcc
              nestedClassesAcc
          PostOp operatorId operator ->
            buildClass
              classDocs
              rest
              constantsAcc
              staticFunctionsAcc
              memberFunctionsAcc
              equalityFunctionAcc
              comparisonFunctionAcc
              negationFunctionAcc
              absFunctionAcc
              preOperatorsAcc
              (addPostOverload operatorId (PostOperator operator) postOperatorsAcc)
              nestedClassesAcc
          Nested nestedDocstring nestedMembers ->
            buildClass
              classDocs
              rest
              constantsAcc
              staticFunctionsAcc
              memberFunctionsAcc
              equalityFunctionAcc
              comparisonFunctionAcc
              negationFunctionAcc
              absFunctionAcc
              preOperatorsAcc
              postOperatorsAcc
              (nestedClassesAcc + [class_ nestedDocstring nestedMembers])

----- FUNCTION COLLECTION -----

functions :: List Function
functions = List.collect classFunctions classes

constantFunctionInfo :: FFI.Id a -> (Name, Constant) -> Function
constantFunctionInfo classId_ (constantName, const@(Constant value _)) =
  Function
    { ffiName = Constant.ffiName classId_ constantName
    , constraint = Nothing
    , argumentTypes = []
    , returnType = Constant.valueType value
    , invoke = Constant.invoke const
    }

staticFunctionInfo :: FFI.Id a -> (Name, StaticFunction) -> Function
staticFunctionInfo classId_ (functionName, staticFunction) = do
  let (constraint, arguments, returnType) = StaticFunction.signature staticFunction
  Function
    { ffiName = StaticFunction.ffiName classId_ functionName staticFunction
    , constraint
    , argumentTypes = List.map Pair.second arguments
    , returnType
    , invoke = StaticFunction.invoke staticFunction
    }

memberFunctionInfo :: FFI.Id value -> (Name, MemberFunction value) -> Function
memberFunctionInfo classId_ (functionName, memberFunction) = do
  let (constraint, arguments, selfType, returnType) = MemberFunction.signature memberFunction
  Function
    { ffiName = MemberFunction.ffiName classId_ functionName memberFunction
    , constraint
    , argumentTypes = List.map Pair.second arguments + [selfType]
    , returnType
    , invoke = MemberFunction.invoke memberFunction
    }

negationFunctionInfo ::
  forall value.
  FFI value =>
  FFI.Id value ->
  Maybe (value -> value) ->
  List Function
negationFunctionInfo classId_ maybeNegationFunction = case maybeNegationFunction of
  Nothing -> []
  Just negationFunction -> do
    let selfType = FFI.typeOf @value Proxy
    List.singleton $
      Function
        { ffiName = NegationFunction.ffiName classId_
        , constraint = Nothing
        , argumentTypes = [selfType]
        , returnType = selfType
        , invoke = NegationFunction.invoke negationFunction
        }

absFunctionInfo ::
  forall value.
  FFI value =>
  FFI.Id value ->
  Maybe (value -> value) ->
  List Function
absFunctionInfo classId_ maybeAbsFunction = case maybeAbsFunction of
  Nothing -> []
  Just absFunction -> do
    let selfType = FFI.typeOf @value Proxy
    List.singleton $
      Function
        { ffiName = AbsFunction.ffiName classId_
        , constraint = Nothing
        , argumentTypes = [selfType]
        , returnType = selfType
        , invoke = AbsFunction.invoke absFunction
        }

equalityFunctionInfo ::
  forall value.
  FFI value =>
  FFI.Id value ->
  Maybe (value -> value -> Bool) ->
  List Function
equalityFunctionInfo classId_ maybeEqualityFunction = case maybeEqualityFunction of
  Nothing -> []
  Just equalityFunction -> do
    let selfType = FFI.typeOf @value Proxy
    List.singleton $
      Function
        { ffiName = EqualityFunction.ffiName classId_
        , constraint = Nothing
        , argumentTypes = [selfType, selfType]
        , returnType = FFI.typeOf @Bool Proxy
        , invoke = EqualityFunction.invoke equalityFunction
        }

comparisonFunctionInfo ::
  forall value.
  FFI value =>
  FFI.Id value ->
  Maybe (value -> value -> Int) ->
  List Function
comparisonFunctionInfo classId_ maybeComparisonFunction = case maybeComparisonFunction of
  Nothing -> []
  Just comparisonFunction -> do
    let selfType = FFI.typeOf @value Proxy
    List.singleton $
      Function
        { ffiName = ComparisonFunction.ffiName classId_
        , constraint = Nothing
        , argumentTypes = [selfType, selfType]
        , returnType = FFI.typeOf @Int Proxy
        , invoke = ComparisonFunction.invoke comparisonFunction
        }

preOperatorOverload :: FFI.Id value -> BinaryOperator.Id -> PreOperator value -> Function
preOperatorOverload classId_ operatorId operator = do
  let (lhsType, selfType, returnType) = PreOperator.signature operator
  Function
    { ffiName = PreOperator.ffiName classId_ operatorId operator
    , constraint = Nothing
    , argumentTypes = [lhsType, selfType]
    , returnType = returnType
    , invoke = PreOperator.invoke operator
    }

preOperatorOverloads ::
  FFI.Id value ->
  (BinaryOperator.Id, List (PreOperator value)) ->
  List Function
preOperatorOverloads classId_ (operatorId, overloads) =
  List.map (preOperatorOverload classId_ operatorId) overloads

postOperatorOverload :: FFI.Id value -> BinaryOperator.Id -> PostOperator value -> Function
postOperatorOverload classId_ operatorId operator = do
  let (selfType, rhsType, returnType) = PostOperator.signature operator
  Function
    { ffiName = PostOperator.ffiName classId_ operatorId operator
    , constraint = Nothing
    , argumentTypes = [selfType, rhsType]
    , returnType = returnType
    , invoke = PostOperator.invoke operator
    }

postOperatorOverloads ::
  FFI.Id value ->
  (BinaryOperator.Id, List (PostOperator value)) ->
  List Function
postOperatorOverloads classId_ (operatorId, overloads) =
  List.map (postOperatorOverload classId_ operatorId) overloads

classFunctions :: Class -> List Function
classFunctions
  ( Class
      classId_
      _
      constants
      staticFunctions
      memberFunctions
      maybeEqualityFunction
      maybeComparisonFunction
      maybeNegationFunction
      maybeAbsFunction
      preOperators
      postOperators
      nestedClasses
    ) =
    List.concat
      [ List.map (constantFunctionInfo classId_) constants
      , List.map (staticFunctionInfo classId_) staticFunctions
      , List.map (memberFunctionInfo classId_) memberFunctions
      , equalityFunctionInfo classId_ maybeEqualityFunction
      , comparisonFunctionInfo classId_ maybeComparisonFunction
      , negationFunctionInfo classId_ maybeNegationFunction
      , absFunctionInfo classId_ maybeAbsFunction
      , List.collect (preOperatorOverloads classId_) preOperators
      , List.collect (postOperatorOverloads classId_) postOperators
      , List.collect classFunctions nestedClasses
      ]
