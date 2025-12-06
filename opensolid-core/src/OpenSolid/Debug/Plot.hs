module OpenSolid.Debug.Plot
  ( viewBox
  , xAxis
  , yAxis
  , curve
  , curveWith
  )
where

import OpenSolid.Bounds2d (Bounds2d)
import OpenSolid.Bounds2d qualified as Bounds2d
import OpenSolid.Curve (Curve)
import OpenSolid.Curve qualified as Curve
import OpenSolid.Curve2d qualified as Curve2d
import OpenSolid.Length (Length)
import OpenSolid.Length qualified as Length
import OpenSolid.Point2d (Point2d)
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Resolution (Resolution)
import OpenSolid.Resolution qualified as Resolution
import OpenSolid.Svg (Svg)
import OpenSolid.Svg qualified as Svg

data Space

resolution :: Resolution Meters
resolution = Resolution.maxError (Length.millimeters 0.1)

scale :: Quantity (Meters ?/? Unitless)
scale = Length.centimeters 10 ?/ 1

axisHeadLength :: Length
axisHeadLength = Length.millimeters 3

axisHeadWidth :: Length
axisHeadWidth = Length.millimeters 2

viewBox :: Point2d Unitless Space -> Point2d Unitless Space -> Bounds2d Meters Space
viewBox p1 p2 = Bounds2d.hull2 (Point2d.convert scale p1) (Point2d.convert scale p2)

xAxis :: Number -> Number -> Svg Space
xAxis x1 x2 =
  Svg.arrow
    (#start (Point2d.x (Quantity.convert scale x1)))
    (#end (Point2d.x (Quantity.convert scale x2 .+. axisHeadLength)))
    (#headLength axisHeadLength)
    (#headWidth axisHeadWidth)

yAxis :: Number -> Number -> Svg Space
yAxis y1 y2 =
  Svg.arrow
    (#start (Point2d.y (Quantity.convert scale y1)))
    (#end (Point2d.y (Quantity.convert scale y2 .+. axisHeadLength)))
    (#headLength axisHeadLength)
    (#headWidth axisHeadWidth)

curve :: Curve Unitless -> Svg Space
curve = curveWith []

curveWith :: List (Svg.Attribute Space) -> Curve Unitless -> Svg Space
curveWith attributes givenCurve = do
  let curve2d = Curve2d.xy Curve.t givenCurve
  Svg.curveWith attributes resolution (Curve2d.convert scale curve2d)
