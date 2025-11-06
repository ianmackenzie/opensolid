module Tests.Direction2d (tests) where

import OpenSolid.Angle qualified as Angle
import OpenSolid.Direction2d qualified as Direction2d
import OpenSolid.Prelude
import OpenSolid.Tolerance qualified as Tolerance
import Test (Test)
import Test qualified

tests :: List Test
tests =
  [ angleFrom
  ]

angleFrom :: Test
angleFrom = do
  let testCase startDegrees endDegrees expectedDegrees =
        Test.verify "angleFrom" do
          let startDirection = Direction2d.degrees startDegrees
          let endDirection = Direction2d.degrees endDegrees
          let computedAngle = Direction2d.angleFrom startDirection endDirection
          let expectedAngle = Angle.degrees expectedDegrees
          Tolerance.using (Angle.radians 1e-12) do
            Test.expect (computedAngle ~= expectedAngle)
  Test.group "angleFrom" $
    [ testCase 10 30 20
    , testCase 10 350 -20
    ]
