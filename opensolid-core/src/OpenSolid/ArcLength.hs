module OpenSolid.ArcLength (parameterization) where

import OpenSolid.Bezier qualified as Bezier
import OpenSolid.Lobatto qualified as Lobatto
import OpenSolid.Number qualified as Number
import OpenSolid.Prelude
import OpenSolid.Quantity (Quantity (Quantity#))
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Sign qualified as Sign
import OpenSolid.Unboxed.Math

data Tree units
  = Node (Tree units) (Quantity units) (Tree units)
  | Leaf (Quantity units) Double# Double# Double# Double# Double# Double#

parameterization ::
  Tolerance units =>
  (Number -> Quantity units) ->
  (Number -> Quantity units) ->
  (Number -> Number, Quantity units)
parameterization dldt d2ldt2 = do
  let dldt1 = dldt 0.0
  let dldt2 = dldt Lobatto.p2
  let dldt3 = dldt Lobatto.p3
  let dldt4 = dldt 1.0
  if
    | isConstant dldt1 dldt2 dldt3 dldt4 -> (id, dldt1)
    | isLinear dldt1 dldt2 dldt3 dldt4 -> do
        let delta = dldt4 - dldt1
        let t0 = -dldt1 / delta
        let sign = Sign.value (Quantity.sign delta)
        let function s = t0 + sign * Number.sqrt (t0 * t0 + (1.0 - 2.0 * t0) * s)
        let length = 0.5 * (dldt1 + dldt4)
        (function, length)
    | otherwise -> do
        let coarseEstimate = Lobatto.estimate dldt1 dldt2 dldt3 dldt4
        let (tree, length) = buildTree 1 dldt d2ldt2 0.0 1.0 dldt1 dldt4 coarseEstimate
        let function s = evaluateIn tree (s * length)
        (function, length)

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
  let dy = y4 - y1
  y2 ~= y1 + dy * Lobatto.p2
    && y3 ~= y1 + dy * Lobatto.p3

evaluateIn :: Tree units -> Quantity units -> Number
evaluateIn tree length = case tree of
  Node leftTree leftLength rightTree
    | length < leftLength -> evaluateIn leftTree length
    | otherwise -> evaluateIn rightTree (length - leftLength)
  Leaf segmentLength t1# t2# t3# t4# t5# t6# -> do
    let !(Quantity# s#) = length / segmentLength
    Quantity# (quinticBezier# t1# t2# t3# t4# t5# t6# s#)

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
buildTree level dldt d2ldt2 tStart tEnd dldtStart dldtEnd coarseEstimate = do
  let tMid = Number.midpoint tStart tEnd
  let dldtMid = dldt tMid
  let dldtLeft2 = dldt (Number.interpolateFrom tStart tMid Lobatto.p2)
  let dldtLeft3 = dldt (Number.interpolateFrom tStart tMid Lobatto.p3)
  let dldtRight2 = dldt (Number.interpolateFrom tMid tEnd Lobatto.p2)
  let dldtRight3 = dldt (Number.interpolateFrom tMid tEnd Lobatto.p3)
  let halfWidth = 0.5 * (tEnd - tStart)
  let leftEstimate = halfWidth * Lobatto.estimate dldtStart dldtLeft2 dldtLeft3 dldtMid
  let rightEstimate = halfWidth * Lobatto.estimate dldtMid dldtRight2 dldtRight3 dldtEnd
  let fineEstimate = leftEstimate + rightEstimate
  if level >= 10 || Quantity.abs (fineEstimate - coarseEstimate) <= 1e-12 * fineEstimate
    then do
      let segmentLength = fineEstimate
      let dtdsStart = segmentLength / dldtStart
      let dtdsEnd = segmentLength / dldtEnd
      let d2tds2Start = -segmentLength ?*? d2ldt2 tStart * dtdsStart / Quantity.squared_ dldtStart
      let d2tds2End = -segmentLength ?*? d2ldt2 tEnd * dtdsEnd / Quantity.squared_ dldtEnd
      let !(Quantity# t1#, Quantity# t2#, Quantity# t3#, Quantity# t4#, Quantity# t5#, Quantity# t6#) =
            Bezier.quinticHermite tStart dtdsStart d2tds2Start tEnd dtdsEnd d2tds2End
      (Leaf segmentLength t1# t2# t3# t4# t5# t6#, segmentLength)
    else do
      let (leftTree, leftLength) =
            buildTree (level + 1) dldt d2ldt2 tStart tMid dldtStart dldtMid leftEstimate
      let (rightTree, rightLength) =
            buildTree (level + 1) dldt d2ldt2 tMid tEnd dldtMid dldtEnd rightEstimate
      (Node leftTree leftLength rightTree, leftLength + rightLength)
