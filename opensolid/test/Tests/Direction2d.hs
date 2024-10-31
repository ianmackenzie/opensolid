module Tests.Direction2d (tests) where

import Angle qualified
import Direction2d qualified
import OpenSolid
import Test (Test)
import Test qualified
import Tolerance qualified

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
    [ testCase 10.0 30.0 20.0
    , testCase 10.0 350.0 -20.0
    ]
