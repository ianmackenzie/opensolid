module OpenSolid.UvPoint (UvPoint) where

import OpenSolid.Prelude
import OpenSolid.Primitives (Point2D)
import OpenSolid.UvSpace (UvSpace)

type UvPoint = Point2D Unitless UvSpace
