module OpenSolid.ArcLength (parameterization) where

import OpenSolid.Curve (Curve)
import OpenSolid.Curve qualified as Curve
import OpenSolid.Float qualified as Float
import OpenSolid.Lobatto qualified as Lobatto
import OpenSolid.Prelude
import OpenSolid.Qty qualified as Qty
import OpenSolid.Range (Range (Range))
import OpenSolid.Range qualified as Range
import OpenSolid.Text qualified as Text
import Prelude qualified

data Tree units
  = Node (Tree units) (Qty units) (Tree units)
  | Leaf (Qty units) (Curve Unitless)

parameterization :: Curve units -> (Curve Unitless, Qty units)
parameterization derivativeMagnitude = do
  let dsdt = Curve.evaluate derivativeMagnitude
  let d2sdt2 = Curve.evaluate (Curve.derivative derivativeMagnitude)
  let dsdt1 = dsdt 0.0
  let dsdt2 = dsdt Lobatto.p2
  let dsdt3 = dsdt Lobatto.p3
  let dsdt4 = dsdt 1.0
  let coarseEstimate = Lobatto.estimate dsdt1 dsdt2 dsdt3 dsdt4
  let (tree, length) = buildTree 1 dsdt d2sdt2 0.0 1.0 dsdt1 dsdt4 coarseEstimate
  let curve = Curve.new (Parameterization derivativeMagnitude tree length)
  (curve, length)

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
      let tCurve = Curve.hermite (tStart, [dtduStart, d2tdu2Start]) (tEnd, [dtduEnd, d2tdu2End])
      (Leaf deltaS tCurve, fineEstimate)
    else do
      let (leftTree, leftLength) =
            buildTree (level + 1) dsdt d2sdt2 tStart tMid dsdtStart dsdtMid leftEstimate
      let (rightTree, rightLength) =
            buildTree (level + 1) dsdt d2sdt2 tMid tEnd dsdtMid dsdtEnd rightEstimate
      (Node leftTree leftLength rightTree, leftLength + rightLength)

data Parameterization where
  Parameterization :: Curve units -> Tree units -> Qty units -> Parameterization

instance Show Parameterization where
  show _ = Text.unpack "ArcLength.Parameterization"

instance Curve.Interface Parameterization Unitless where
  evaluateImpl (Parameterization _ tree length) uValue = lookup tree (uValue * length)

  evaluateBoundsImpl curve (Range uLow uHigh) =
    Range.from (Curve.evaluateImpl curve uLow) (Curve.evaluateImpl curve uHigh)

  derivativeImpl tCurve@(Parameterization dsdt _ length) = length / (dsdt . Curve.new tCurve)

lookup :: Tree units -> Qty units -> Float
lookup tree length = case tree of
  Node leftTree leftLength rightTree
    | length < leftLength -> lookup leftTree length
    | otherwise -> lookup rightTree (length - leftLength)
  Leaf segmentLength curve -> Curve.evaluate curve (length / segmentLength)
