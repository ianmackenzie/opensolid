module OpenSolid.UvCurve
  ( UvCurve
  , pointAt
  , pointOn
  , nondegenerate
  )
where

import OpenSolid.Curve qualified as Curve
import OpenSolid.Curve2D (Curve2D)
import OpenSolid.Curve2D qualified as Curve2D
import OpenSolid.IsDegenerate (IsDegenerate)
import OpenSolid.Nondegenerate (Nondegenerate)
import OpenSolid.Prelude
import OpenSolid.UvPoint (UvPoint)

type UvCurve = Curve2D Unitless

pointAt :: Number -> UvCurve -> UvPoint
pointAt = Curve2D.pointAt

pointOn :: UvCurve -> Number -> UvPoint
pointOn = Curve2D.pointOn

nondegenerate :: UvCurve -> Result IsDegenerate (Nondegenerate UvCurve)
nondegenerate = unitless Curve.nondegenerate
