module Tests.Direction2d (tests) where

import Angle qualified
import Direction2d qualified
import OpenSolid
import Test (Test)
import Test qualified

tests :: List Test
tests =
  [ angleFrom
  ]

angleFrom :: Test
angleFrom =
  Test.group "angleFrom" <|
    [ testCase 10.0 30.0 20.0
    , testCase 10.0 350.0 -20.0
    ]
 where
  testCase startDegrees endDegrees expectedDegrees =
    Test.verify "angleFrom" do
      let startDirection = Direction2d.degrees startDegrees
      let endDirection = Direction2d.degrees endDegrees
      let computedAngle = Direction2d.angleFrom startDirection endDirection
      let expectedAngle = Angle.degrees expectedDegrees
      Test.expect (let ?tolerance = Angle.radians 1e-12 in computedAngle ~= expectedAngle)
