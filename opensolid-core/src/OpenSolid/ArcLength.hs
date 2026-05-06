module OpenSolid.ArcLength (parameterization) where

import OpenSolid.Bezier qualified as Bezier
import OpenSolid.Interval qualified as Interval
import OpenSolid.Lobatto qualified as Lobatto
import OpenSolid.Number qualified as Number
import OpenSolid.Prelude
import OpenSolid.Quantity (Quantity (Q#))
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Unboxed.Math

data Tree units
  = Node (Quantity units) (Tree units) (Tree units)
  | Leaf (Quantity units) Double# Double# Double# Double# Double# Double#
  | SingularStart (Quantity units) Double# Double# Double# Double#
  | SingularEnd (Quantity units) Double# Double# Double# Double#

parameterization ::
  (Number -> Quantity units) ->
  (Number -> Quantity units) ->
  (Quantity units, Number -> Number)
parameterization dsdt d2sdt2 = do
  let dsdt1 = dsdt 0.0
  let dsdt2 = dsdt Lobatto.p2
  let dsdt3 = dsdt Lobatto.p3
  let dsdt4 = dsdt 1.0
  let dsdtMin = dsdt1 `min` dsdt2 `min` dsdt3 `min` dsdt4
  let dsdtMax = dsdt1 `max` dsdt2 `max` dsdt3 `max` dsdt4
  let dsdtTolerance = Tolerance.unitless * dsdtMax -- Reasonable tolerance for comparing ds/dt values
  let (isConstant, singular0, singular1) =
        Tolerance.using dsdtTolerance do
          (dsdtMin ~= dsdtMax, dsdt1 ~= Quantity.zero, dsdt4 ~= Quantity.zero)
  if isConstant
    then (dsdt1, id) -- Constant-speed curve (e.g. line or arc)
    else do
      let initialEstimate = Lobatto.estimate dsdt1 dsdt2 dsdt3 dsdt4
      let (length, tree) =
            buildTree 1 dsdt d2sdt2 singular0 singular1 0.0 1.0 dsdt1 dsdt4 initialEstimate
      let uniformParameterizationValue r = evaluate tree (length * r)
      (length, uniformParameterizationValue)

evaluate :: Tree units -> Quantity units -> Number
evaluate tree s = case tree of
  Node leftLength leftTree rightTree
    | s < leftLength -> evaluate leftTree s
    | otherwise -> evaluate rightTree (s - leftLength)
  Leaf segmentLength t1# t2# t3# t4# t5# t6# -> do
    let !(Q# r#) = Quantity.clampTo Interval.unit (s / segmentLength)
    Q# (quinticBezier# t1# t2# t3# t4# t5# t6# r#)
  SingularStart segmentLength q1# q2# q3# q4# -> do
    let !(Q# r#) = Quantity.clampTo Interval.unit (s / segmentLength)
    Q# (sqrt# (cubicBezier# q1# q2# q3# q4# r#))
  SingularEnd segmentLength q1# q2# q3# q4# -> do
    let !(Q# r#) = 1.0 - Quantity.clampTo Interval.unit (s / segmentLength)
    Q# (1.0## -# sqrt# (cubicBezier# q1# q2# q3# q4# r#))

buildTree ::
  Int ->
  (Number -> Quantity units) ->
  (Number -> Quantity units) ->
  Bool ->
  Bool ->
  Number ->
  Number ->
  Quantity units ->
  Quantity units ->
  Quantity units ->
  (Quantity units, Tree units)
buildTree n dsdt d2sdt2 singular0 singular1 tStart tEnd dsdtStart dsdtEnd coarseEstimate = do
  let tMid = Number.midpoint tStart tEnd
  let dsdtMid = dsdt tMid
  let dsdtLeft2 = dsdt (Number.interpolateFrom tStart tMid Lobatto.p2)
  let dsdtLeft3 = dsdt (Number.interpolateFrom tStart tMid Lobatto.p3)
  let dsdtRight2 = dsdt (Number.interpolateFrom tMid tEnd Lobatto.p2)
  let dsdtRight3 = dsdt (Number.interpolateFrom tMid tEnd Lobatto.p3)
  let halfWidth = 0.5 * (tEnd - tStart)
  let leftEstimate = halfWidth * Lobatto.estimate dsdtStart dsdtLeft2 dsdtLeft3 dsdtMid
  let rightEstimate = halfWidth * Lobatto.estimate dsdtMid dsdtRight2 dsdtRight3 dsdtEnd
  let fineEstimate = leftEstimate + rightEstimate
  let converged = Quantity.abs (fineEstimate - coarseEstimate) <= 1e-12 * fineEstimate
  let singularStart = singular0 && tStart == 0.0
  let singularEnd = singular1 && tEnd == 1.0
  if (n >= 10 || converged) && not (singularStart && singularEnd)
    then do
      let d2sdt2Start = d2sdt2 tStart
      let d2sdt2End = d2sdt2 tEnd
      let segmentLength = fineEstimate
      if
        | singularStart -> do
            let qEnd = Quantity.squared tEnd
            let dqdrStart = 2.0 * segmentLength / d2sdt2Start
            let dqdrEnd = 2.0 * tEnd * segmentLength / dsdtEnd
            let !(Q# q1#, Q# q2#, Q# q3#, Q# q4#) =
                  Bezier.cubicHermite 0.0 dqdrStart qEnd dqdrEnd
            (segmentLength, SingularStart segmentLength q1# q2# q3# q4#)
        | singularEnd -> do
            let qStart = Quantity.squared (1.0 - tStart)
            let dqdrStart = 2.0 * (1.0 - tStart) * segmentLength / dsdtStart
            let dqdrEnd = -2.0 * segmentLength / d2sdt2End
            let !(Q# q1#, Q# q2#, Q# q3#, Q# q4#) =
                  Bezier.cubicHermite 0.0 dqdrEnd qStart dqdrStart
            (segmentLength, SingularEnd segmentLength q1# q2# q3# q4#)
        | otherwise -> do
            let dtdrStart = segmentLength / dsdtStart
            let dtdrEnd = segmentLength / dsdtEnd
            let d2tdr2Start =
                  -segmentLength ?*? d2sdt2Start * dtdrStart / Quantity.squared_ dsdtStart
            let d2tdr2End =
                  -segmentLength ?*? d2sdt2End * dtdrEnd / Quantity.squared_ dsdtEnd
            let !(Q# t1#, Q# t2#, Q# t3#, Q# t4#, Q# t5#, Q# t6#) =
                  Bezier.quinticHermite tStart dtdrStart d2tdr2Start tEnd dtdrEnd d2tdr2End
            (segmentLength, Leaf segmentLength t1# t2# t3# t4# t5# t6#)
    else do
      let (leftLength, leftTree) =
            buildTree (n + 1) dsdt d2sdt2 singular0 singular1 tStart tMid dsdtStart dsdtMid leftEstimate
      let (rightLength, rightTree) =
            buildTree (n + 1) dsdt d2sdt2 singular0 singular1 tMid tEnd dsdtMid dsdtEnd rightEstimate
      (leftLength + rightLength, Node leftLength leftTree rightTree)
