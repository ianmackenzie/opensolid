module OpenSolid.ArcLength (parameterization) where

import OpenSolid.Bezier qualified as Bezier
import OpenSolid.Lobatto qualified as Lobatto
import OpenSolid.Number qualified as Number
import OpenSolid.Prelude
import OpenSolid.Quantity (Quantity (Q#))
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Unboxed.Math

data Tree units
  = Node (Quantity units) (Tree units) (Tree units)
  | Leaf (Quantity units) Double# Double# Double# Double# Double# Double#

parameterization ::
  (Number -> Quantity units) ->
  (Number -> Quantity units) ->
  (Quantity units, Quantity units -> Number)
parameterization dldt d2ldt2 = do
  let dldt1 = dldt 0.0
  let dldt2 = dldt Lobatto.p2
  let dldt3 = dldt Lobatto.p3
  let dldt4 = dldt 1.0
  let coarseEstimate = Lobatto.estimate dldt1 dldt2 dldt3 dldt4
  let (length, tree) = build 1 dldt d2ldt2 0.0 1.0 dldt1 dldt4 coarseEstimate
  (length, evaluate tree)

evaluate :: Tree units -> Quantity units -> Number
evaluate tree length = case tree of
  Node leftLength leftTree rightTree
    | length < leftLength -> evaluate leftTree length
    | otherwise -> evaluate rightTree (length - leftLength)
  Leaf segmentLength t1# t2# t3# t4# t5# t6# -> do
    let !(Q# s#) = length / segmentLength
    Q# (quinticBezier# t1# t2# t3# t4# t5# t6# s#)

build ::
  Int ->
  (Number -> Quantity units) ->
  (Number -> Quantity units) ->
  Number ->
  Number ->
  Quantity units ->
  Quantity units ->
  Quantity units ->
  (Quantity units, Tree units)
build level dldt d2ldt2 tStart tEnd dldtStart dldtEnd coarseEstimate = do
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
      let !(Q# t1#, Q# t2#, Q# t3#, Q# t4#, Q# t5#, Q# t6#) =
            Bezier.quinticHermite tStart dtdsStart d2tds2Start tEnd dtdsEnd d2tds2End
      (segmentLength, Leaf segmentLength t1# t2# t3# t4# t5# t6#)
    else do
      let (leftLength, leftTree) =
            build (level + 1) dldt d2ldt2 tStart tMid dldtStart dldtMid leftEstimate
      let (rightLength, rightTree) =
            build (level + 1) dldt d2ldt2 tMid tEnd dldtMid dldtEnd rightEstimate
      (leftLength + rightLength, Node leftLength leftTree rightTree)
