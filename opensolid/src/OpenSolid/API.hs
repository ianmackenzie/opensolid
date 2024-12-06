module OpenSolid.API (Class (..), Function (..), classes, functions) where

import Angle qualified
import Bounds2d (Bounds2d)
import Bounds2d qualified
import Colour (Colour)
import Colour qualified
import Curve1d (Curve1d)
import Curve1d qualified
import Curve1d.Root qualified
import Data.Proxy (Proxy (Proxy))
import Direction2d (Direction2d)
import Direction2d qualified
import Foreign (Ptr)
import Length (Length)
import Length qualified
import List qualified
import OpenSolid
import OpenSolid.API.BinaryOperator qualified as BinaryOperator
import OpenSolid.API.ComparisonFunction qualified as ComparisonFunction
import OpenSolid.API.Constant (Constant (Constant))
import OpenSolid.API.Constant qualified as Constant
import OpenSolid.API.Constraint (Constraint)
import OpenSolid.API.EqualityFunction qualified as EqualityFunction
import OpenSolid.API.MemberFunction (MemberFunction (..))
import OpenSolid.API.MemberFunction qualified as MemberFunction
import OpenSolid.API.Name (Name)
import OpenSolid.API.Name qualified as Name
import OpenSolid.API.NegationOperator qualified as NegationOperator
import OpenSolid.API.PostOperator (PostOperator (PostOperator))
import OpenSolid.API.PostOperator qualified as PostOperator
import OpenSolid.API.PreOperator (PreOperator (PreOperator))
import OpenSolid.API.PreOperator qualified as PreOperator
import OpenSolid.API.StaticFunction (StaticFunction (..))
import OpenSolid.API.StaticFunction qualified as StaticFunction
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import Pair qualified
import Point2d (Point2d)
import Point2d qualified
import Range (Range)
import Range qualified
import Units (Meters)
import Vector2d (Vector2d)
import Vector2d qualified

data Space

data Class where
  Class ::
    FFI value =>
    { id :: FFI.Id value
    , constants :: List (Name, Constant)
    , staticFunctions :: List (Name, List StaticFunction)
    , memberFunctions :: List (Name, List (MemberFunction value))
    , equalityFunction :: Maybe (value -> value -> Bool)
    , comparisonFunction :: Maybe (value -> value -> Int)
    , negationFunction :: Maybe (value -> value)
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
  List.concat
    [ length
    , angle
    , range
    , color
    , vector2d
    , direction2d
    , point2d
    , bounds2d
    , curve1d
    ]

length :: List Class
length =
  [ class_ @Length
      [ constant "Zero" Length.zero
      , static1 "Meters" "Value" Length.meters
      , static1 "Centimeters" "Value" Length.centimeters
      , static1 "Millimeters" "Value" Length.millimeters
      , static1 "Inches" "Value" Length.inches
      , member0 "In Meters" Length.inMeters
      , member0 "In Centimeters" Length.inCentimeters
      , member0 "In Millimeters" Length.inMillimeters
      , member0 "In Inches" Length.inInches
      , equality
      , comparison
      , negateSelf
      , floatTimes
      , plusSelf
      , plus @(Range Meters) Self
      , plus @(Curve1d Meters) Self
      , minusSelf
      , minus @(Range Meters) Self
      , minus @(Curve1d Meters) Self
      , timesFloat
      , times @(Range Unitless) Self
      , times @(Curve1d Unitless) Self
      , divByFloat
      , divBySelf
      , divBy @(Range Unitless) Self
      , divBy @(Range Meters) Self
      , divBy @(Curve1d Unitless) Self
      , divBy @(Curve1d Meters) Self
      , floorDivBySelf
      , modBySelf
      ]
  ]

angle :: List Class
angle =
  [ class_ @Angle
      [ constant "Zero" Angle.zero
      , static1 "Radians" "Value" Angle.radians
      , static1 "Degrees" "Value" Angle.degrees
      , static1 "Turns" "Value" Angle.turns
      , member0 "In Radians" Angle.inRadians
      , member0 "In Degrees" Angle.inDegrees
      , member0 "In Turns" Angle.inTurns
      , equality
      , comparison
      , negateSelf
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
  ]

data Range_

instance FFI Range_ where
  representation = FFI.abstractClassRepresentation "Range"

range :: List Class
range =
  [ class_ @Range_
      [ constant "Unit" Range.unit
      , static1 "Constant" "Value" (Range.constant @Unitless)
      , static1 "Constant" "Value" (Range.constant @Radians)
      , static1 "Constant" "Value" (Range.constant @Meters)
      , static2 "From Endpoints" "A" "B" (Range.from @Unitless)
      , static2 "From Endpoints" "A" "B" (Range.from @Radians)
      , static2 "From Endpoints" "A" "B" (Range.from @Meters)
      , static2 "Meters" "A" "B" Range.meters
      , static2 "Centimeters" "A" "B" Range.centimeters
      , static2 "Millimeters" "A" "B" Range.millimeters
      , static2 "Inches" "A" "B" Range.inches
      , static2 "Aggregate" "A" "B" (Range.aggregate2 @Unitless)
      , static2 "Aggregate" "A" "B" (Range.aggregate2 @Radians)
      , static2 "Aggregate" "A" "B" (Range.aggregate2 @Meters)
      , static3 "Aggregate" "A" "B" "C" (Range.aggregate3 @Unitless)
      , static3 "Aggregate" "A" "B" "C" (Range.aggregate3 @Radians)
      , static3 "Aggregate" "A" "B" "C" (Range.aggregate3 @Meters)
      ]
  , class_ @(Range Unitless)
      [ member0 "Endpoints" Range.endpoints
      , member1 "Intersection" "Other" Range.intersection
      , member1 "Contains" "Value" Range.includes
      , member1 "Contains" "Other" Range.contains
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
      , times @Angle Self
      , divByFloat
      , divBySelf
      ]
  , class_ @(Range Radians)
      [ member0 "Endpoints" Range.endpoints
      , member1 "Intersection" "Other" Range.intersection
      , member1 "Contains" "Value" Range.includes
      , member1 "Contains" "Other" Range.contains
      , negateSelf
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
  , class_ @(Range Meters)
      [ member0 "Endpoints" Range.endpoints
      , member1 "Intersection" "Other" Range.intersection
      , member1 "Contains" "Value" Range.includes
      , member1 "Contains" "Other" Range.contains
      , negateSelf
      , floatTimes
      , plusSelf
      , plus @Length Self
      , minusSelf
      , minus @Length Self
      , timesFloat
      , divByFloat
      , divBySelf
      , divBy @(Range Unitless) Self
      ]
  ]

color :: List Class
color =
  [ class_ @Colour
      [ static3 "RGB" "Red" "Green" "Blue" Colour.rgb
      , static3 "RGB255" "Red" "Green" "Blue" Colour.rgb255
      , static3 "HSL" "Hue" "Saturation" "Lightness" Colour.hsl
      , static1 "From Hex" "Hex String" Colour.fromHex
      , member0 "To Hex" Colour.toHex
      , member0 "Components" Colour.components
      , member0 "Components255" Colour.components255
      , constant "Red" Colour.red
      , constant "Dark Red" Colour.darkRed
      , constant "Light Orange" Colour.lightOrange
      , constant "Orange" Colour.orange
      , constant "Dark Orange" Colour.darkOrange
      , constant "Light Yellow" Colour.lightYellow
      , constant "Yellow" Colour.yellow
      , constant "Dark Yellow" Colour.darkYellow
      , constant "Light Green" Colour.lightGreen
      , constant "Green" Colour.green
      , constant "Dark Green" Colour.darkGreen
      , constant "Light Blue" Colour.lightBlue
      , constant "Blue" Colour.blue
      , constant "Dark Blue" Colour.darkBlue
      , constant "Light Purple" Colour.lightPurple
      , constant "Purple" Colour.purple
      , constant "Dark Purple" Colour.darkPurple
      , constant "Light Brown" Colour.lightBrown
      , constant "Brown" Colour.brown
      , constant "Dark Brown" Colour.darkBrown
      , constant "Black" Colour.black
      , constant "White" Colour.white
      , constant "Light Grey" Colour.lightGrey
      , constant "Grey" Colour.grey
      , constant "Dark Grey" Colour.darkGrey
      , constant "Light Gray" Colour.lightGray
      , constant "Gray" Colour.gray
      , constant "Dark Gray" Colour.darkGray
      , constant "Light Charcoal" Colour.lightCharcoal
      , constant "Charcoal" Colour.charcoal
      , constant "Dark Charcoal" Colour.darkCharcoal
      ]
  ]

data Vector2d_

instance FFI Vector2d_ where
  representation = FFI.abstractClassRepresentation "Vector2d"

vector2d :: List Class
vector2d =
  [ class_ @Vector2d_
      [ constant "Zero" (Vector2d.zero @Space @Meters)
      , static1 "Unit" "Direction" Vector2d.unit
      , static2 "Meters" "X Component" "Y Component" Vector2d.meters
      , static2 "Centimeters" "X Component" "Y Component" Vector2d.centimeters
      , static2 "Millimeters" "X Component" "Y Component" Vector2d.millimeters
      , static2 "Inches" "X Component" "Y Component" Vector2d.inches
      , static2 "XY" "X Component" "Y Component" (Vector2d.xy @Space @Unitless)
      , static2 "XY" "X Component" "Y Component" (Vector2d.xy @Space @Meters)
      , static1 "X" "X Component" (Vector2d.x @Space @Unitless)
      , static1 "X" "X Component" (Vector2d.x @Space @Meters)
      , static1 "Y" "Y Component" (Vector2d.y @Space @Unitless)
      , static1 "Y" "Y Component" (Vector2d.y @Space @Meters)
      , static1 "From Components" "Components" (Vector2d.fromComponents @Space @Unitless)
      , static1 "From Components" "Components" (Vector2d.fromComponents @Space @Meters)
      ]
  , class_ @(Vector2d (Space @ Unitless))
      [ member0 "Components" Vector2d.components
      , member0 "X Component" Vector2d.xComponent
      , member0 "Y Component" Vector2d.yComponent
      , memberU0 "Direction" Vector2d.direction
      , negateSelf
      , floatTimes
      , plusSelf
      , minusSelf
      , timesFloat
      , times @Length Self
      , divByFloat
      ]
  , class_ @(Vector2d (Space @ Meters))
      [ member0 "Components" Vector2d.components
      , member0 "X Component" Vector2d.xComponent
      , member0 "Y Component" Vector2d.yComponent
      , memberM0 "Direction" Vector2d.direction
      , negateSelf
      , floatTimes
      , plusSelf
      , minusSelf
      , timesFloat
      , divByFloat
      , divBy @Length Self
      ]
  ]

direction2d :: List Class
direction2d =
  [ class_ @(Direction2d Space)
      [ constant "X" Direction2d.x
      , constant "Y" Direction2d.y
      , constant "Positive X" Direction2d.positiveX
      , constant "Positive Y" Direction2d.positiveY
      , constant "Negative X" Direction2d.negativeX
      , constant "Negative Y" Direction2d.negativeY
      , static1 "From Angle" "Angle" Direction2d.fromAngle
      , member0 "To Angle" Direction2d.toAngle
      , member0 "Components" Direction2d.components
      , member0 "X Component" Direction2d.xComponent
      , member0 "Y Component" Direction2d.yComponent
      , negateSelf
      ]
  ]

data Point2d_

instance FFI Point2d_ where
  representation = FFI.abstractClassRepresentation "Point2d"

point2d :: List Class
point2d =
  [ class_ @Point2d_
      [ constant "Origin" (Point2d.origin @Space @Meters)
      , static2 "XY" "X Coordinate" "Y Coordinate" (Point2d.xy @Space @Unitless)
      , static2 "XY" "X Coordinate" "Y Coordinate" (Point2d.xy @Space @Meters)
      , static1 "X" "X Coordinate" (Point2d.x @Space @Unitless)
      , static1 "X" "X Coordinate" (Point2d.x @Space @Meters)
      , static1 "Y" "Y Coordinate" (Point2d.y @Space @Unitless)
      , static1 "Y" "Y Coordinate" (Point2d.y @Space @Meters)
      , static2 "Meters" "X Coordinate" "Y Coordinate" Point2d.meters
      , static2 "Centimeters" "X Coordinate" "Y Coordinate" Point2d.centimeters
      , static2 "Millimeters" "X Coordinate" "Y Coordinate" Point2d.millimeters
      , static2 "Inches" "X Coordinate" "Y Coordinate" Point2d.inches
      , static1 "From Coordinates" "Coordinates" (Point2d.fromCoordinates @Space @Unitless)
      , static1 "From Coordinates" "Coordinates" (Point2d.fromCoordinates @Space @Meters)
      ]
  , class_ @(Point2d (Space @ Unitless))
      [ member0 "Coordinates" Point2d.coordinates
      , member0 "X Coordinate" Point2d.xCoordinate
      , member0 "Y Coordinate" Point2d.yCoordinate
      , member1 "Distance To" "Other" Point2d.distanceFrom
      , member1 "Midpoint" "Other" Point2d.midpoint
      ]
  , class_ @(Point2d (Space @ Meters))
      [ member0 "Coordinates" Point2d.coordinates
      , member0 "X Coordinate" Point2d.xCoordinate
      , member0 "Y Coordinate" Point2d.yCoordinate
      , member1 "Distance To" "Other" Point2d.distanceFrom
      , member1 "Midpoint" "Other" Point2d.midpoint
      ]
  ]

data Bounds2d_

instance FFI Bounds2d_ where
  representation = FFI.abstractClassRepresentation "Bounds2d"

bounds2d :: List Class
bounds2d =
  [ class_ @Bounds2d_
      [ static2 "XY" "X Coordinate" "Y Coordinate" (Bounds2d.xy @Space @Unitless)
      , static2 "XY" "X Coordinate" "Y Coordinate" (Bounds2d.xy @Space @Meters)
      , static1 "Constant" "Point" (Bounds2d.constant @Space @Unitless)
      , static1 "Constant" "Point" (Bounds2d.constant @Space @Meters)
      , static2 "Hull" "P1" "P2" (Bounds2d.hull2 @Space @Unitless)
      , static2 "Hull" "P1" "P2" (Bounds2d.hull2 @Space @Meters)
      , static3 "Hull" "P1" "P2" "P3" (Bounds2d.hull3 @Space @Unitless)
      , static3 "Hull" "P1" "P2" "P3" (Bounds2d.hull3 @Space @Meters)
      , static4 "Hull" "P1" "P2" "P3" "P4" (Bounds2d.hull4 @Space @Unitless)
      , static4 "Hull" "P1" "P2" "P3" "P4" (Bounds2d.hull4 @Space @Meters)
      ]
  , class_ @(Bounds2d (Space @ Unitless))
      [ member0 "Coordinates" Bounds2d.coordinates
      , member0 "X Coordinate" Bounds2d.xCoordinate
      , member0 "Y Coordinate" Bounds2d.yCoordinate
      ]
  , class_ @(Bounds2d (Space @ Meters))
      [ member0 "Coordinates" Bounds2d.coordinates
      , member0 "X Coordinate" Bounds2d.xCoordinate
      , member0 "Y Coordinate" Bounds2d.yCoordinate
      ]
  ]

data Curve1d_

instance FFI Curve1d_ where
  representation = FFI.abstractClassRepresentation "Curve1d"

curve1d :: List Class
curve1d =
  [ class_ @Curve1d_
      [ constant "T" Curve1d.t
      , static1 "Sin" "Curve" Curve1d.sin
      , static1 "Cos" "Curve" Curve1d.cos
      , static1 "Sqrt" "Curve" (Curve1d.sqrt @Unitless)
      , nested @Curve1d.Root
          [ member0 "Value" Curve1d.Root.value
          , member0 "Order" Curve1d.Root.order
          , member0 "Sign" (\root -> 1 * Curve1d.Root.sign root) -- TODO return as enum?
          ]
      ]
  , class_ @(Curve1d Unitless)
      [ member0 "Squared" Curve1d.squared
      , member1 "Evaluate" "Parameter Value" (\t curve -> Curve1d.evaluate curve t)
      , memberU0 "Zeros" Curve1d.zeros
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
      , times @Angle Self
      , times @(Curve1d Meters) Self
      , times @(Curve1d Radians) Self
      , divByFloat
      , divBySelf
      ]
  , class_ @(Curve1d Radians)
      [ member1 "Evaluate" "Parameter Value" (\t curve -> Curve1d.evaluate curve t)
      , memberR0 "Zeros" Curve1d.zeros
      , negateSelf
      , floatTimes
      , plusSelf
      , minusSelf
      , timesFloat
      , times @(Curve1d Unitless) Self
      , divByFloat
      , divBySelf
      , divBy @Angle Self
      , divBy @(Curve1d Unitless) Self
      ]
  , class_ @(Curve1d Meters)
      [ member1 "Evaluate" "Parameter Value" (\t curve -> Curve1d.evaluate curve t)
      , memberM0 "Zeros" Curve1d.zeros
      , negateSelf
      , floatTimes
      , plusSelf
      , minusSelf
      , timesFloat
      , times @(Curve1d Unitless) Self
      , divByFloat
      , divBySelf
      , divBy @Length Self
      , divBy @(Curve1d Unitless) Self
      ]
  ]

----- CLASS MEMBERS -----

class_ :: forall value. FFI value => List (Member value) -> Class
class_ members = buildClass members [] [] [] Nothing Nothing Nothing [] [] []

data Member value where
  Const :: FFI result => Text -> result -> Member value
  Static1 :: (FFI a, FFI result) => Text -> Text -> (a -> result) -> Member value
  Static2 :: (FFI a, FFI b, FFI result) => Text -> Text -> Text -> (a -> b -> result) -> Member value
  Static3 :: (FFI a, FFI b, FFI c, FFI result) => Text -> Text -> Text -> Text -> (a -> b -> c -> result) -> Member value
  Static4 :: (FFI a, FFI b, FFI c, FFI d, FFI result) => Text -> Text -> Text -> Text -> Text -> (a -> b -> c -> d -> result) -> Member value
  Member0 :: (FFI value, FFI result) => Text -> (value -> result) -> Member value
  MemberU0 :: (FFI value, FFI result) => Text -> (Tolerance Unitless => value -> result) -> Member value
  MemberR0 :: (FFI value, FFI result) => Text -> (Tolerance Radians => value -> result) -> Member value
  MemberM0 :: (FFI value, FFI result) => Text -> (Tolerance Meters => value -> result) -> Member value
  Member1 :: (FFI a, FFI value, FFI result) => Text -> Text -> (a -> value -> result) -> Member value
  Equality :: Eq value => Member value
  Comparison :: Ord value => Member value
  Negate :: Negation value => Member value
  PreOp :: (FFI lhs, FFI result) => BinaryOperator.Id -> (lhs -> value -> result) -> Member value
  PostOp :: (FFI rhs, FFI result) => BinaryOperator.Id -> (value -> rhs -> result) -> Member value
  Nested :: FFI nested => List (Member nested) -> Member value

constant :: FFI result => Text -> result -> Member value
constant = Const

static1 :: (FFI a, FFI result) => Text -> Text -> (a -> result) -> Member value
static1 = Static1

static2 :: (FFI a, FFI b, FFI result) => Text -> Text -> Text -> (a -> b -> result) -> Member value
static2 = Static2

static3 :: (FFI a, FFI b, FFI c, FFI result) => Text -> Text -> Text -> Text -> (a -> b -> c -> result) -> Member value
static3 = Static3

static4 :: (FFI a, FFI b, FFI c, FFI d, FFI result) => Text -> Text -> Text -> Text -> Text -> (a -> b -> c -> d -> result) -> Member value
static4 = Static4

member0 :: (FFI value, FFI result) => Text -> (value -> result) -> Member value
member0 = Member0

memberU0 :: (FFI value, FFI result) => Text -> (Tolerance Unitless => value -> result) -> Member value
memberU0 = MemberU0

memberR0 :: (FFI value, FFI result) => Text -> (Tolerance Radians => value -> result) -> Member value
memberR0 = MemberR0

memberM0 :: (FFI value, FFI result) => Text -> (Tolerance Meters => value -> result) -> Member value
memberM0 = MemberM0

member1 :: (FFI a, FFI value, FFI result) => Text -> Text -> (a -> value -> result) -> Member value
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

nested :: FFI nested => List (Member nested) -> Member value
nested = Nested

addStaticOverload ::
  Name ->
  StaticFunction ->
  List (Name, List StaticFunction) ->
  List (Name, List StaticFunction)
addStaticOverload name overload [] = [(name, [overload])]
addStaticOverload name overload (first : rest) = do
  let (existingName, existingOverloads) = first
  if name == existingName
    then (existingName, existingOverloads + [overload]) : rest
    else first : addStaticOverload name overload rest

addMemberOverload ::
  Name ->
  MemberFunction value ->
  List (Name, List (MemberFunction value)) ->
  List (Name, List (MemberFunction value))
addMemberOverload name overload [] = [(name, [overload])]
addMemberOverload name overload (first : rest) = do
  let (existingName, existingOverloads) = first
  if name == existingName
    then (existingName, existingOverloads + [overload]) : rest
    else first : addMemberOverload name overload rest

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
  List (Member value) ->
  List (Name, Constant) ->
  List (Name, List StaticFunction) ->
  List (Name, List (MemberFunction value)) ->
  Maybe (value -> value -> Bool) ->
  Maybe (value -> value -> Int) ->
  Maybe (value -> value) ->
  List (BinaryOperator.Id, List (PreOperator value)) ->
  List (BinaryOperator.Id, List (PostOperator value)) ->
  List Class ->
  Class
buildClass
  members
  constantsAcc
  staticFunctionsAcc
  memberFunctionsAcc
  equalityFunctionAcc
  comparisonFunctionAcc
  negationFunctionAcc
  preOperatorsAcc
  postOperatorsAcc
  nestedClassesAcc =
    case members of
      [] ->
        Class
          { id = case FFI.typeOf @value Proxy of
              FFI.Class (FFI.Id Proxy names maybeUnits) -> FFI.Id Proxy names maybeUnits
              _ -> internalError "Every class defined in the API must correspond to an FFI type with class representation"
          , constants = constantsAcc
          , staticFunctions = staticFunctionsAcc
          , memberFunctions = memberFunctionsAcc
          , equalityFunction = equalityFunctionAcc
          , comparisonFunction = comparisonFunctionAcc
          , negationFunction = negationFunctionAcc
          , preOperators = preOperatorsAcc
          , postOperators = postOperatorsAcc
          , nestedClasses = nestedClassesAcc
          }
      first : rest -> do
        let addStatic name overload =
              buildClass
                rest
                constantsAcc
                (addStaticOverload (Name.parse name) overload staticFunctionsAcc)
                memberFunctionsAcc
                equalityFunctionAcc
                comparisonFunctionAcc
                negationFunctionAcc
                preOperatorsAcc
                postOperatorsAcc
                nestedClassesAcc
        let addMember name overload =
              buildClass
                rest
                constantsAcc
                staticFunctionsAcc
                (addMemberOverload (Name.parse name) overload memberFunctionsAcc)
                equalityFunctionAcc
                comparisonFunctionAcc
                negationFunctionAcc
                preOperatorsAcc
                postOperatorsAcc
                nestedClassesAcc
        case first of
          Const name value ->
            buildClass
              rest
              (constantsAcc + [(Name.parse name, Constant value)])
              staticFunctionsAcc
              memberFunctionsAcc
              equalityFunctionAcc
              comparisonFunctionAcc
              negationFunctionAcc
              preOperatorsAcc
              postOperatorsAcc
              nestedClassesAcc
          Static1 name arg1 f ->
            addStatic name (StaticFunction1 (Name.parse arg1) f)
          Static2 name arg1 arg2 f ->
            addStatic name (StaticFunction2 (Name.parse arg1) (Name.parse arg2) f)
          Static3 name arg1 arg2 arg3 f ->
            addStatic name (StaticFunction3 (Name.parse arg1) (Name.parse arg2) (Name.parse arg3) f)
          Static4 name arg1 arg2 arg3 arg4 f ->
            addStatic name (StaticFunction4 (Name.parse arg1) (Name.parse arg2) (Name.parse arg3) (Name.parse arg4) f)
          Member0 name f ->
            addMember name (MemberFunction0 f)
          MemberU0 name f ->
            addMember name (MemberFunction0U f)
          MemberR0 name f ->
            addMember name (MemberFunction0R f)
          MemberM0 name f ->
            addMember name (MemberFunction0M f)
          Member1 name arg1 f ->
            addMember name (MemberFunction1 (Name.parse arg1) f)
          Equality ->
            buildClass
              rest
              constantsAcc
              staticFunctionsAcc
              memberFunctionsAcc
              (Just (==))
              comparisonFunctionAcc
              negationFunctionAcc
              preOperatorsAcc
              postOperatorsAcc
              nestedClassesAcc
          Comparison ->
            buildClass
              rest
              constantsAcc
              staticFunctionsAcc
              memberFunctionsAcc
              equalityFunctionAcc
              (Just comparisonImpl)
              negationFunctionAcc
              preOperatorsAcc
              postOperatorsAcc
              nestedClassesAcc
          Negate ->
            buildClass
              rest
              constantsAcc
              staticFunctionsAcc
              memberFunctionsAcc
              equalityFunctionAcc
              comparisonFunctionAcc
              (Just negate)
              preOperatorsAcc
              postOperatorsAcc
              nestedClassesAcc
          PreOp operatorId operator ->
            buildClass
              rest
              constantsAcc
              staticFunctionsAcc
              memberFunctionsAcc
              equalityFunctionAcc
              comparisonFunctionAcc
              negationFunctionAcc
              (addPreOverload operatorId (PreOperator operator) preOperatorsAcc)
              postOperatorsAcc
              nestedClassesAcc
          PostOp operatorId operator ->
            buildClass
              rest
              constantsAcc
              staticFunctionsAcc
              memberFunctionsAcc
              equalityFunctionAcc
              comparisonFunctionAcc
              negationFunctionAcc
              preOperatorsAcc
              (addPostOverload operatorId (PostOperator operator) postOperatorsAcc)
              nestedClassesAcc
          Nested nestedMembers ->
            buildClass
              rest
              constantsAcc
              staticFunctionsAcc
              memberFunctionsAcc
              equalityFunctionAcc
              comparisonFunctionAcc
              negationFunctionAcc
              preOperatorsAcc
              postOperatorsAcc
              (nestedClassesAcc + [class_ nestedMembers])

----- FUNCTION COLLECTION -----

functions :: List Function
functions = List.collect classFunctions classes

constantFunction :: FFI.Id a -> (Name, Constant) -> Function
constantFunction classId_ (constantName, const@(Constant value)) =
  Function
    { ffiName = Constant.ffiName classId_ constantName
    , constraint = Nothing
    , argumentTypes = []
    , returnType = Constant.valueType value
    , invoke = Constant.invoke const
    }

staticFunctionOverload :: FFI.Id a -> Name -> StaticFunction -> Function
staticFunctionOverload classId_ functionName staticFunction = do
  let (constraint, arguments, returnType) = StaticFunction.signature staticFunction
  Function
    { ffiName = StaticFunction.ffiName classId_ functionName staticFunction
    , constraint
    , argumentTypes = List.map Pair.second arguments
    , returnType
    , invoke = StaticFunction.invoke staticFunction
    }

staticFunctionOverloads :: FFI.Id a -> (Name, List StaticFunction) -> List Function
staticFunctionOverloads classId_ (functionName, overloads) =
  List.map (staticFunctionOverload classId_ functionName) overloads

memberFunctionOverload :: FFI.Id value -> Name -> MemberFunction value -> Function
memberFunctionOverload classId_ functionName memberFunction = do
  let (constraint, arguments, selfType, returnType) = MemberFunction.signature memberFunction
  Function
    { ffiName = MemberFunction.ffiName classId_ functionName memberFunction
    , constraint
    , argumentTypes = List.map Pair.second arguments + [selfType]
    , returnType
    , invoke = MemberFunction.invoke memberFunction
    }

memberFunctionOverloads :: FFI.Id value -> (Name, List (MemberFunction value)) -> List Function
memberFunctionOverloads classId_ (functionName, overloads) =
  List.map (memberFunctionOverload classId_ functionName) overloads

negationOperatorInfo ::
  forall value.
  FFI value =>
  FFI.Id value ->
  Maybe (value -> value) ->
  List Function
negationOperatorInfo classId_ maybeNegationFunction = case maybeNegationFunction of
  Nothing -> []
  Just negationFunction -> do
    let selfType = FFI.typeOf @value Proxy
    List.singleton $
      Function
        { ffiName = NegationOperator.ffiName classId_
        , constraint = Nothing
        , argumentTypes = [selfType]
        , returnType = selfType
        , invoke = NegationOperator.invoke negationFunction
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
      constants
      staticFunctions
      memberFunctions
      maybeEqualityFunction
      maybeComparisonFunction
      maybeNegationOperator
      preOperators
      postOperators
      nestedClasses
    ) =
    List.concat
      [ List.map (constantFunction classId_) constants
      , List.collect (staticFunctionOverloads classId_) staticFunctions
      , List.collect (memberFunctionOverloads classId_) memberFunctions
      , equalityFunctionInfo classId_ maybeEqualityFunction
      , comparisonFunctionInfo classId_ maybeComparisonFunction
      , negationOperatorInfo classId_ maybeNegationOperator
      , List.collect (preOperatorOverloads classId_) preOperators
      , List.collect (postOperatorOverloads classId_) postOperators
      , List.collect classFunctions nestedClasses
      ]
