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
    Test.check 1 "angleFrom" <|
      let startDirection = Direction2d.degrees startDegrees
          endDirection = Direction2d.degrees endDegrees
          computedAngle = Direction2d.angleFrom startDirection endDirection
          expectedAngle = Angle.degrees expectedDegrees
       in Test.expect (let ?tolerance = Angle.radians 1e-12 in computedAngle ~= expectedAngle)
