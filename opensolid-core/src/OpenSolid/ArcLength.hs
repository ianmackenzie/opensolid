module OpenSolid.ArcLength (parameterization) where

import OpenSolid.Interval (Interval (Interval))
import OpenSolid.CompiledFunction qualified as CompiledFunction
import OpenSolid.Curve1D (Curve1D)
import OpenSolid.Curve1D qualified as Curve1D
import OpenSolid.DivisionByZero (DivisionByZero (DivisionByZero))
import OpenSolid.Lobatto qualified as Lobatto
import OpenSolid.Number qualified as Number
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Tolerance qualified as Tolerance

data Tree units
  = Node (Tree units) (Quantity units) (Tree units)
  | Leaf (Quantity units) (Curve1D Unitless)

parameterization :: Tolerance units => Curve1D units -> (Curve1D Unitless, Quantity units)
parameterization derivativeMagnitude = do
  let dsdt = Curve1D.evaluate derivativeMagnitude
  let d2sdt2 = Curve1D.evaluate derivativeMagnitude.derivative
  let dsdt1 = dsdt 0
  let dsdt2 = dsdt Lobatto.p2
  let dsdt3 = dsdt Lobatto.p3
  let dsdt4 = dsdt 1
  if
    | isConstant dsdt1 dsdt2 dsdt3 dsdt4 -> (Curve1D.t, dsdt1)
    | isLinear dsdt1 dsdt2 dsdt3 dsdt4 -> do
        let delta = dsdt4 .-. dsdt1
        let t0 = negative dsdt1 ./. delta
        let sqrt = Tolerance.using Quantity.zero do
              Curve1D.sqrt (t0 .*. t0 .+. (1 -. 2 *. t0) .*. Curve1D.t)
        let curve = if delta > Quantity.zero then t0 .+. sqrt else t0 .-. sqrt
        let length = 0.5 *. (dsdt1 .+. dsdt4)
        (curve, length)
    | otherwise -> do
        let coarseEstimate = Lobatto.estimate dsdt1 dsdt2 dsdt3 dsdt4
        let (tree, length) = buildTree 1 dsdt d2sdt2 0 1 dsdt1 dsdt4 coarseEstimate
        let evaluate uValue = evaluateIn tree (uValue .*. length)
        let evaluateBounds (Interval uLow uHigh) = Interval (evaluate uLow) (evaluate uHigh)
        let compiled = CompiledFunction.abstract evaluate evaluateBounds
        case Curve1D.quotient (Curve1D.constant length) derivativeMagnitude of
          Ok quotient -> (Curve1D.recursive compiled (quotient `compose`), length)
          Error DivisionByZero -> (Curve1D.t, Quantity.zero)

isConstant ::
  Tolerance units =>
  Quantity units ->
  Quantity units ->
  Quantity units ->
  Quantity units ->
  Bool
isConstant y1 y2 y3 y4 = y1 ~= y2 && y1 ~= y3 && y1 ~= y4

isLinear ::
  Tolerance units =>
  Quantity units ->
  Quantity units ->
  Quantity units ->
  Quantity units ->
  Bool
isLinear y1 y2 y3 y4 = do
  let dy = y4 .-. y1
  y2 ~= y1 .+. dy .*. Lobatto.p2
    && y3 ~= y1 .+. dy .*. Lobatto.p3

evaluateIn :: Tree units -> Quantity units -> Number
evaluateIn tree length = case tree of
  Node leftTree leftLength rightTree
    | length < leftLength -> evaluateIn leftTree length
    | otherwise -> evaluateIn rightTree (length .-. leftLength)
  Leaf segmentLength curve -> Curve1D.evaluate curve (length ./. segmentLength)

buildTree ::
  Int ->
  (Number -> Quantity units) ->
  (Number -> Quantity units) ->
  Number ->
  Number ->
  Quantity units ->
  Quantity units ->
  Quantity units ->
  (Tree units, Quantity units)
buildTree level dsdt d2sdt2 tStart tEnd dsdtStart dsdtEnd coarseEstimate = do
  let tMid = Number.midpoint tStart tEnd
  let dsdtMid = dsdt tMid
  let dsdtLeft2 = dsdt (Number.interpolateFrom tStart tMid Lobatto.p2)
  let dsdtLeft3 = dsdt (Number.interpolateFrom tStart tMid Lobatto.p3)
  let dsdtRight2 = dsdt (Number.interpolateFrom tMid tEnd Lobatto.p2)
  let dsdtRight3 = dsdt (Number.interpolateFrom tMid tEnd Lobatto.p3)
  let halfWidth = 0.5 *. (tEnd .-. tStart)
  let leftEstimate = halfWidth .*. Lobatto.estimate dsdtStart dsdtLeft2 dsdtLeft3 dsdtMid
  let rightEstimate = halfWidth .*. Lobatto.estimate dsdtMid dsdtRight2 dsdtRight3 dsdtEnd
  let fineEstimate = leftEstimate .+. rightEstimate
  if level >= 10 || Quantity.abs (fineEstimate .-. coarseEstimate) <= 1e-12 *. fineEstimate
    then do
      let deltaS = fineEstimate
      let dtduStart = deltaS ./. dsdtStart
      let dtduEnd = deltaS ./. dsdtEnd
      let d2tdu2Start = negative deltaS ?*? d2sdt2 tStart .*. dtduStart ./. Quantity.squared_ dsdtStart
      let d2tdu2End = negative deltaS ?*? d2sdt2 tEnd .*. dtduEnd ./. Quantity.squared_ dsdtEnd
      let tCurve = Curve1D.hermite tStart [dtduStart, d2tdu2Start] tEnd [dtduEnd, d2tdu2End]
      (Leaf deltaS tCurve, fineEstimate)
    else do
      let (leftTree, leftLength) =
            buildTree (level + 1) dsdt d2sdt2 tStart tMid dsdtStart dsdtMid leftEstimate
      let (rightTree, rightLength) =
            buildTree (level + 1) dsdt d2sdt2 tMid tEnd dsdtMid dsdtEnd rightEstimate
      (Node leftTree leftLength rightTree, leftLength .+. rightLength)
