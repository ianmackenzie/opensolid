module Tests.Arc2d (tests) where

import Angle qualified
import Arc2d qualified
import Curve2d qualified
import Float qualified
import OpenSolid
import Point2d (Point2d)
import Point2d qualified
import Test (Test)
import Test qualified
import Units (Meters)

tests :: Tolerance Meters => List Test
tests =
  [ from
  ]

from :: Tolerance Meters => Test
from =
  let test :: String -> Angle -> Point2d (space @ Meters) -> Test
      test label angle expectedPoint = Test.verify label $ Test.do
        arc <- Arc2d.swept angle Point2d.origin (Point2d.meters 1.0 1.0)
        Test.expect (Curve2d.evaluateAt 0.5 arc ~= expectedPoint)
      invSqrt2 = 1.0 / Float.sqrt 2.0
   in Test.group "from" $
        [ test "90 degrees CCW" Angle.quarterTurn $
            Point2d.meters invSqrt2 (1.0 - invSqrt2)
        , test "90 degrees CW" -Angle.quarterTurn $
            Point2d.meters (1.0 - invSqrt2) invSqrt2
        , test "180 degrees CCW" Angle.halfTurn $
            Point2d.meters 1.0 0.0
        , test "180 degrees CW" -Angle.halfTurn $
            Point2d.meters 0.0 1.0
        ]
