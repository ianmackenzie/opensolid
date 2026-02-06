module OpenSolid.Curve.Search
  ( Tree
  , tree
  , curve
  , tangentCurve
  )
where

import {-# SOURCE #-} OpenSolid.Curve (Curve)
import {-# SOURCE #-} OpenSolid.Curve qualified as Curve
import OpenSolid.Curve.Segment (Segment)
import OpenSolid.Curve.Segment qualified as Segment
import {-# SOURCE #-} OpenSolid.DirectionCurve (DirectionCurve)
import {-# SOURCE #-} OpenSolid.DirectionCurve qualified as DirectionCurve
import OpenSolid.Interval (Interval)
import OpenSolid.Prelude
import OpenSolid.Search qualified as Search
import {-# SOURCE #-} OpenSolid.VectorCurve qualified as VectorCurve

type Tree dimension units space =
  Search.Tree (Interval Unitless) (Segment dimension units space)

tree ::
  ( Curve.Exists dimension units space
  , VectorCurve.Exists dimension units space
  , DirectionCurve.Exists dimension space
  , Tolerance units
  ) =>
  Curve dimension units space ->
  Result Curve.IsPoint (Tree dimension units space)
tree givenCurve = do
  computedTangentCurve <- Curve.tangentDirection givenCurve
  Ok (Search.tree (Segment.evaluate givenCurve computedTangentCurve) Search.curveDomain)

curve :: Tree dimension units space -> Curve dimension units space
curve (Search.Tree _ segment _) = Segment.curve segment

tangentCurve :: Tree dimension units space -> DirectionCurve dimension space
tangentCurve (Search.Tree _ segment _) = Segment.tangentCurve segment
