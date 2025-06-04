module Tests.Polygon2d (tests) where

import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Polygon2d qualified as Polygon2d
import OpenSolid.Prelude
import Test (Test)
import Test qualified

tests :: Tolerance Meters => List Test
tests = [fromVertices]

fromVertices :: Tolerance Meters => Test
fromVertices = Test.verify "fromVertices" do
  let p1 = Point2d.origin
  let p2 = Point2d.meters 1.0 0.0
  let p3 = Point2d.meters 1.0 1.0
  let p4 = Point2d.meters 0.0 1.0
  Test.all
    [ case Polygon2d.fromVertices (NonEmpty.four p1 p2 p3 p4) of
        Success polygon -> Test.expect (polygon.vertices == NonEmpty.four p1 p2 p3 p4)
        Failure _ -> Test.fail "Polygon construction should succeed"
    , case Polygon2d.fromVertices (NonEmpty.one p1) of
        Success _ -> Test.fail "Polygon construction from single point should fail"
        Failure _ -> Test.pass
    , case Polygon2d.fromVertices (NonEmpty.five p1 p2 p3 p4 p1) of
        Success polygon -> Test.expect (polygon.vertices == NonEmpty.four p1 p2 p3 p4)
        Failure _ -> Test.fail "Polygon construction with end==start should succeed"
    , case Polygon2d.fromVertices (NonEmpty.five p1 p2 p2 p3 p4) of
        Success _ -> Test.fail "Polygon construction with duplicate inner point should fail"
        Failure _ -> Test.pass
    ]
