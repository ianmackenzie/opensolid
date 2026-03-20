module OpenSolid.Curve.Search (Tree, tree) where

import OpenSolid.Bounds qualified as Bounds
import {-# SOURCE #-} OpenSolid.Curve (Curve)
import {-# SOURCE #-} OpenSolid.Curve qualified as Curve
import OpenSolid.Curve.Segment (Segment)
import OpenSolid.Curve.Segment qualified as Segment
import OpenSolid.DirectionBounds qualified as DirectionBounds
import OpenSolid.Interval (Interval)
import OpenSolid.Prelude
import OpenSolid.Search qualified as Search
import OpenSolid.VectorBounds qualified as VectorBounds
import OpenSolid.VectorCurve qualified as VectorCurve

type Tree dimension units space =
  Search.Tree (Interval Unitless) (Segment dimension units space)

tree ::
  ( Curve.Exists dimension units space
  , VectorCurve.Exists dimension units space
  , Bounds.Exists dimension units space
  , VectorBounds.Exists dimension units space
  , VectorBounds.Exists dimension (Unitless ?/? units) space
  , DirectionBounds.Exists dimension space
  ) =>
  Curve dimension units space ->
  Tree dimension units space
tree curve = Search.tree (Segment.new curve) Search.curveDomain
