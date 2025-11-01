module Main (main) where

import OpenSolid.Color qualified as Color
import OpenSolid.Curve qualified as Curve
import OpenSolid.Debug.Plot qualified as Plot
import OpenSolid.Drawing2d qualified as Drawing2d
import OpenSolid.IO qualified as IO
import OpenSolid.Point2d (Point2d (Point2d))
import OpenSolid.Syntax (float, half, twice, (.*), (.+), (.-))
import OpenSolid.Text qualified as Text

main :: IO ()
main = do
  let t = Curve.t
  let a0 = float 1.0 .- Curve.cubed t .* (float 4.0 .- float 3.0 .* t)
  let b0 = t .* (float 1.0 .+ Curve.squared t .* (twice t .- float 3.0))
  let c0 = Curve.squared t .* (float 0.5 .+ t .* (half t .- float 1.0))
  let a1 = Curve.cubed t .* (float 4.0 .- float 3.0 .* t)
  let b1 = Curve.cubed t .* (t .- float 1.0)
  let drawing yScale a0' b0' c0' a1' b1' =
        Drawing2d.group
          [ Plot.xAxis 0.0 1.0
          , Plot.yAxis 0.0 yScale
          , Plot.curveWith [Drawing2d.strokeColor Color.red] (yScale .* a0')
          , Plot.curveWith [Drawing2d.strokeColor Color.orange] (yScale .* b0')
          , Plot.curveWith [Drawing2d.strokeColor Color.yellow] (yScale .* c0')
          , Plot.curveWith [Drawing2d.strokeColor Color.green] (yScale .* a1')
          , Plot.curveWith [Drawing2d.strokeColor Color.blue] (yScale .* b1')
          , Plot.curveWith [Drawing2d.strokeColor Color.grey] (yScale .* (a0' .+ a1'))
          ]
  let curvesDrawing = drawing 1.0 a0 b0 c0 a1 b1
  let curvesDrawing2 = drawing 1.0 Curve.b00 Curve.b01 Curve.b02 Curve.b10 Curve.b11
  let derivativesDrawing =
        drawing
          0.5
          a0.derivative
          b0.derivative
          c0.derivative
          a1.derivative
          b1.derivative
  let derivativesDrawing2 =
        drawing
          0.5
          Curve.b00.derivative
          Curve.b01.derivative
          Curve.b02.derivative
          Curve.b10.derivative
          Curve.b11.derivative
  let secondDerivativesDrawing =
        drawing
          0.1
          a0.derivative.derivative
          b0.derivative.derivative
          c0.derivative.derivative
          a1.derivative.derivative
          b1.derivative.derivative
  let secondDerivativesDrawing2 =
        drawing
          0.1
          Curve.b00.derivative.derivative
          Curve.b01.derivative.derivative
          Curve.b02.derivative.derivative
          Curve.b10.derivative.derivative
          Curve.b11.derivative.derivative
  let thirdDerivativesDrawing =
        drawing
          0.04
          a0.derivative.derivative.derivative
          b0.derivative.derivative.derivative
          c0.derivative.derivative.derivative
          a1.derivative.derivative.derivative
          b1.derivative.derivative.derivative
  let thirdDerivativesDrawing2 =
        drawing
          0.04
          Curve.b00.derivative.derivative.derivative
          Curve.b01.derivative.derivative.derivative
          Curve.b02.derivative.derivative.derivative
          Curve.b10.derivative.derivative.derivative
          Curve.b11.derivative.derivative.derivative
  let fourthDerivativesDrawing =
        drawing
          0.02
          a0.derivative.derivative.derivative.derivative
          b0.derivative.derivative.derivative.derivative
          c0.derivative.derivative.derivative.derivative
          a1.derivative.derivative.derivative.derivative
          b1.derivative.derivative.derivative.derivative
  let fourthDerivativesDrawing2 =
        drawing
          0.02
          Curve.b00.derivative.derivative.derivative.derivative
          Curve.b01.derivative.derivative.derivative.derivative
          Curve.b02.derivative.derivative.derivative.derivative
          Curve.b10.derivative.derivative.derivative.derivative
          Curve.b11.derivative.derivative.derivative.derivative
  let viewBox y1 y2 = Plot.viewBox (Point2d -0.1 y1) (Point2d 1.1 y2)
  Drawing2d.writeSvg "executables/blending-curves/curves.svg" (viewBox -0.5 1.5) curvesDrawing
  Drawing2d.writeSvg "executables/blending-curves/curves2.svg" (viewBox -0.5 1.5) curvesDrawing2
  Drawing2d.writeSvg "executables/blending-curves/derivatives.svg" (viewBox -1.5 1.5) derivativesDrawing
  Drawing2d.writeSvg "executables/blending-curves/derivatives2.svg" (viewBox -1.5 1.5) derivativesDrawing2
  Drawing2d.writeSvg "executables/blending-curves/second-derivatives.svg" (viewBox -1.5 1.5) secondDerivativesDrawing
  Drawing2d.writeSvg "executables/blending-curves/second-derivatives2.svg" (viewBox -1.5 1.5) secondDerivativesDrawing2
  Drawing2d.writeSvg "executables/blending-curves/third-derivatives.svg" (viewBox -1.5 1.5) thirdDerivativesDrawing
  Drawing2d.writeSvg "executables/blending-curves/third-derivatives2.svg" (viewBox -1.5 1.5) thirdDerivativesDrawing2
  Drawing2d.writeSvg "executables/blending-curves/fourth-derivatives.svg" (viewBox -1.5 1.5) fourthDerivativesDrawing
  Drawing2d.writeSvg "executables/blending-curves/fourth-derivatives2.svg" (viewBox -1.5 1.5) fourthDerivativesDrawing2
  IO.printLine ("a0 fourth derivative: " <> Text.float (Curve.evaluate a0.derivative.derivative.derivative.derivative 0.0))
  IO.printLine ("b0 fourth derivative: " <> Text.float (Curve.evaluate b0.derivative.derivative.derivative.derivative 0.0))
  IO.printLine ("c0 fourth derivative: " <> Text.float (Curve.evaluate c0.derivative.derivative.derivative.derivative 0.0))
  IO.printLine ("a1 fourth derivative: " <> Text.float (Curve.evaluate a1.derivative.derivative.derivative.derivative 0.0))
  IO.printLine ("b1 fourth derivative: " <> Text.float (Curve.evaluate b1.derivative.derivative.derivative.derivative 0.0))
  IO.printLine ("Curve.b00 fourth derivative: " <> Text.float (Curve.evaluate Curve.b00.derivative.derivative.derivative.derivative 0.0))
  IO.printLine ("Curve.b01 fourth derivative: " <> Text.float (Curve.evaluate Curve.b01.derivative.derivative.derivative.derivative 0.0))
  IO.printLine ("Curve.b02 fourth derivative: " <> Text.float (Curve.evaluate Curve.b02.derivative.derivative.derivative.derivative 0.0))
  IO.printLine ("Curve.b10 fourth derivative: " <> Text.float (Curve.evaluate Curve.b10.derivative.derivative.derivative.derivative 0.0))
  IO.printLine ("Curve.b11 fourth derivative: " <> Text.float (Curve.evaluate Curve.b11.derivative.derivative.derivative.derivative 0.0))
