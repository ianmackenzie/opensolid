import Hedgehog (Property)
import Hedgehog qualified
import Hedgehog.Gen qualified
import Hedgehog.Range qualified
import Length qualified
import List qualified
import OpenSolid
import Pair qualified
import Range (Range (..))
import Range qualified
import Script qualified
import Text qualified
import Vector3d qualified
import VectorBox3d (VectorBox3d (VectorBox3d))
import VectorBox3d qualified
import Prelude qualified
import Length (Length)
import Units (Meters)

group :: Text -> List (Text, Property) -> Hedgehog.Group
group name properties =
  Hedgehog.Group (Text.toString name) (List.map (Pair.mapFirst Text.toString) properties)

tolerance :: Length
tolerance = Length.meters 1e-12

parameterValue :: Hedgehog.Gen Float
parameterValue = Hedgehog.Gen.realFloat (Hedgehog.Range.constant 0.0 1.0)

length :: Hedgehog.Gen Length
length = Prelude.fmap Length.meters (Hedgehog.Gen.realFloat (Hedgehog.Range.constantFrom 0.0 -5.0 5.0))

lengthRange :: Hedgehog.Gen (Range Meters)
lengthRange = Prelude.do
  a <- length
  b <- length
  Prelude.return (Range.from a b)

vectorBox3d :: Hedgehog.Gen (VectorBox3d coordinates Meters)
vectorBox3d = Prelude.do
  x <- lengthRange
  y <- lengthRange
  z <- lengthRange
  Prelude.return (VectorBox3d x y z)

quantityContainedIn :: Range Meters -> Length -> Bool
quantityContainedIn (Range low high) value =
  value >= low - tolerance && value <= high + tolerance

withNumTests :: Int -> Hedgehog.Property -> Hedgehog.Property
withNumTests count = Hedgehog.withTests (Prelude.fromIntegral count)

testVectorBox3dMagnitude :: Hedgehog.Property
testVectorBox3dMagnitude =
  Prelude.do
    vectorBox <- Hedgehog.forAll vectorBox3d
    u <- Hedgehog.forAll parameterValue
    v <- Hedgehog.forAll parameterValue
    w <- Hedgehog.forAll parameterValue
    let vector = VectorBox3d.interpolate vectorBox u v w
    let magnitude = Vector3d.magnitude vector
    let magnitudeRange = VectorBox3d.magnitude vectorBox
    Hedgehog.assert (quantityContainedIn magnitudeRange magnitude)
    |> Hedgehog.property
    |> withNumTests 1000

tests :: Hedgehog.Group
tests =
  group
    "Tests"
    [ ("VectorBox3d.magnitude", testVectorBox3dMagnitude)
    ]

script :: Script.Program
script = do
  success <- Script.perform (Hedgehog.checkParallel tests)
  if success then Script.succeed () else Script.fail "Test failed"

main :: IO ()
main = Script.run script
