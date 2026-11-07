module OpenSolid.Surface3D.Intersections.Segment (Segment (..)) where

import OpenSolid.Bisection qualified as Bisection
import OpenSolid.Bounded (Bounded)
import OpenSolid.Bounded qualified as Bounded
import OpenSolid.Bounds3D (Bounds3D)
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Prelude
import OpenSolid.Surface3D qualified as Surface3D
import OpenSolid.Surface3D.Intersections.Domain (Domain)

data Segment space = Segment
  { domain :: Domain
  , range :: ~(Bounds3D space)
  , segments1 :: List (Surface3D.Segment space)
  , segments2 :: List (Surface3D.Segment space)
  , pairs :: List (Surface3D.Segment space, Surface3D.Segment space)
  , frontNeighbor :: Maybe (Segment space)
  , backNeighbor :: Maybe (Segment space)
  , rightNeighbor :: Maybe (Segment space)
  , leftNeighbor :: Maybe (Segment space)
  , topNeighbor :: Maybe (Segment space)
  , bottomNeighbor :: Maybe (Segment space)
  , bottomBackLeftChild :: ~(Segment space)
  , bottomBackRightChild :: ~(Segment space)
  , bottomFrontLeftChild :: ~(Segment space)
  , bottomFrontRightChild :: ~(Segment space)
  , topBackLeftChild :: ~(Segment space)
  , topBackRightChild :: ~(Segment space)
  , topFrontLeftChild :: ~(Segment space)
  , topFrontRightChild :: ~(Segment space)
  }

instance Bounded (Segment space) Domain where
  {-# INLINE bounds #-}
  bounds segment = segment.domain

instance space1 ~ space2 => Bounded (Segment space1) (Bounds3D space2) where
  {-# INLINE bounds #-}
  bounds segment = segment.range

instance Bisection.Segment (Segment space) Domain where
  children segment =
    NonEmpty.eight
      segment.bottomBackLeftChild
      segment.bottomBackRightChild
      segment.bottomFrontLeftChild
      segment.bottomFrontRightChild
      segment.topBackLeftChild
      segment.topBackRightChild
      segment.topFrontLeftChild
      segment.topFrontRightChild
