module OpenSolid.Curve.Search (Tree, tree) where

import {-# SOURCE #-} OpenSolid.Curve (Curve)
import {-# SOURCE #-} OpenSolid.Curve qualified as Curve
import OpenSolid.Curve.Segment (Segment)
import OpenSolid.Curve.Segment qualified as Segment
import OpenSolid.DirectionBounds qualified as DirectionBounds
import OpenSolid.Interval (Interval)
import OpenSolid.Prelude
import OpenSolid.Search qualified as Search
import OpenSolid.VectorBounds qualified as VectorBounds

type Tree dimension units space =
  Search.Tree (Interval Unitless) (Segment dimension units space)

tree ::
  ( Curve.Exists dimension units space
  , VectorBounds.Exists dimension units space
  , VectorBounds.Exists dimension (Unitless ?/? units) space
  , DirectionBounds.Exists dimension space
  ) =>
  Curve dimension units space ->
  Tree dimension units space
tree curve = Search.tree (Segment.evaluate curve) Search.curveDomain
