module OpenSolid.ArcLength (parameterization) where

import OpenSolid.Bounds (Bounds (Bounds))
import OpenSolid.CompiledFunction qualified as CompiledFunction
import OpenSolid.Curve (Curve)
import OpenSolid.Curve qualified as Curve
import OpenSolid.DivisionByZero (DivisionByZero (DivisionByZero))
import OpenSolid.Float qualified as Float
import OpenSolid.Lobatto qualified as Lobatto
import OpenSolid.Prelude
import OpenSolid.Qty qualified as Qty
import OpenSolid.Tolerance qualified as Tolerance

data Tree units
  = Node (Tree units) (Qty units) (Tree units)
  | Leaf (Qty units) (Curve Unitless)

parameterization :: Tolerance units => Curve units -> (Curve Unitless, Qty units)
parameterization derivativeMagnitude = do
  let dsdt = Curve.evaluate derivativeMagnitude
  let d2sdt2 = Curve.evaluate derivativeMagnitude.derivative
  let dsdt1 = dsdt 0.0
  let dsdt2 = dsdt Lobatto.p2
  let dsdt3 = dsdt Lobatto.p3
  let dsdt4 = dsdt 1.0
  if
    | isConstant dsdt1 dsdt2 dsdt3 dsdt4 -> (Curve.t, dsdt1)
    | isLinear dsdt1 dsdt2 dsdt3 dsdt4 -> do
        let delta = dsdt4 - dsdt1
        let t0 = -dsdt1 / delta
        let sqrt = Tolerance.using Qty.zero (Curve.sqrt (t0 * t0 + (1.0 - 2.0 * t0) * Curve.t))
        let curve = if delta > Qty.zero then t0 + sqrt else t0 - sqrt
        let length = 0.5 * (dsdt1 + dsdt4)
        (curve, length)
    | otherwise -> do
        let coarseEstimate = Lobatto.estimate dsdt1 dsdt2 dsdt3 dsdt4
        let (tree, length) = buildTree 1 dsdt d2sdt2 0.0 1.0 dsdt1 dsdt4 coarseEstimate
        let evaluate uValue = lookup tree (uValue * length)
        let evaluateBounds (Bounds uLow uHigh) = Bounds (evaluate uLow) (evaluate uHigh)
        let compiled = CompiledFunction.abstract evaluate evaluateBounds
        case Curve.quotient (Curve.constant length) derivativeMagnitude of
          Success quotient -> (Curve.recursive compiled (quotient .), length)
          Failure DivisionByZero -> (Curve.t, Qty.zero)

isConstant :: Tolerance units => Qty units -> Qty units -> Qty units -> Qty units -> Bool
isConstant y1 y2 y3 y4 = y1 ~= y2 && y1 ~= y3 && y1 ~= y4

isLinear :: Tolerance units => Qty units -> Qty units -> Qty units -> Qty units -> Bool
isLinear y1 y2 y3 y4 = let dy = y4 - y1 in y2 ~= y1 + dy * Lobatto.p2 && y3 ~= y1 + dy * Lobatto.p3

lookup :: Tree units -> Qty units -> Float
lookup tree length = case tree of
  Node leftTree leftLength rightTree
    | length < leftLength -> lookup leftTree length
    | otherwise -> lookup rightTree (length - leftLength)
  Leaf segmentLength curve -> Curve.evaluate curve (length / segmentLength)

buildTree ::
  Int ->
  (Float -> Qty units) ->
  (Float -> Qty units) ->
  Float ->
  Float ->
  Qty units ->
  Qty units ->
  Qty units ->
  (Tree units, Qty units)
buildTree level dsdt d2sdt2 tStart tEnd dsdtStart dsdtEnd coarseEstimate = do
  let tMid = Float.midpoint tStart tEnd
  let dsdtMid = dsdt tMid
  let dsdtLeft2 = dsdt (Float.interpolateFrom tStart tMid Lobatto.p2)
  let dsdtLeft3 = dsdt (Float.interpolateFrom tStart tMid Lobatto.p3)
  let dsdtRight2 = dsdt (Float.interpolateFrom tMid tEnd Lobatto.p2)
  let dsdtRight3 = dsdt (Float.interpolateFrom tMid tEnd Lobatto.p3)
  let halfWidth = 0.5 * (tEnd - tStart)
  let leftEstimate = halfWidth * Lobatto.estimate dsdtStart dsdtLeft2 dsdtLeft3 dsdtMid
  let rightEstimate = halfWidth * Lobatto.estimate dsdtMid dsdtRight2 dsdtRight3 dsdtEnd
  let fineEstimate = leftEstimate + rightEstimate
  if level >= 10 || Qty.abs (fineEstimate - coarseEstimate) <= 1e-12 * fineEstimate
    then do
      let deltaS = fineEstimate
      let dtduStart = deltaS / dsdtStart
      let dtduEnd = deltaS / dsdtEnd
      let d2tdu2Start = -deltaS .*. d2sdt2 tStart * dtduStart / Qty.squared' dsdtStart
      let d2tdu2End = -deltaS .*. d2sdt2 tEnd * dtduEnd / Qty.squared' dsdtEnd
      let tCurve = Curve.hermite tStart [dtduStart, d2tdu2Start] tEnd [dtduEnd, d2tdu2End]
      (Leaf deltaS tCurve, fineEstimate)
    else do
      let (leftTree, leftLength) =
            buildTree (level + 1) dsdt d2sdt2 tStart tMid dsdtStart dsdtMid leftEstimate
      let (rightTree, rightLength) =
            buildTree (level + 1) dsdt d2sdt2 tMid tEnd dsdtMid dsdtEnd rightEstimate
      (Node leftTree leftLength rightTree, leftLength + rightLength)
