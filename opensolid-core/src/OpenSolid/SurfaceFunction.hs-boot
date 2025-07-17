-- Allow typeclass instances to be declared here
-- even though the type is actually defined in the Functions module
{-# OPTIONS_GHC -Wno-orphans #-}

module OpenSolid.SurfaceFunction
  ( SurfaceFunction (compiled, du, dv)
  , Compiled
  , new
  , evaluate
  , evaluateBounds
  , compiled
  , derivative
  , constant
  , squared'
  , sqrt'
  , quotient
  , quotient'
  , sin
  , cos
  )
where

import OpenSolid.Bounds (Bounds)
import OpenSolid.Functions
  ( Curve
  , Curve2d
  , SurfaceFunction (..)
  , SurfaceFunction2d
  , SurfaceFunctionCompiled
  )
import OpenSolid.Prelude
import OpenSolid.SurfaceParameter (SurfaceParameter)
import OpenSolid.Units qualified as Units
import OpenSolid.UvBounds (UvBounds)
import OpenSolid.UvPoint (UvPoint)

type Compiled units = SurfaceFunctionCompiled units

new ::
  ( "compiled" ::: Compiled units
  , "derivative" ::: (SurfaceParameter -> SurfaceFunction units)
  , "composeCurve" ::: (Curve2d UvCoordinates -> Curve units)
  , "composeSurfaceFunction" ::: (SurfaceFunction2d UvCoordinates -> SurfaceFunction units)
  ) ->
  SurfaceFunction units
evaluate :: SurfaceFunction units -> UvPoint -> Qty units
evaluateBounds :: SurfaceFunction units -> UvBounds -> Bounds units
compiled :: SurfaceFunction units -> Compiled units
derivative :: SurfaceParameter -> SurfaceFunction units -> SurfaceFunction units
constant :: Qty units -> SurfaceFunction units
squared' :: SurfaceFunction units -> SurfaceFunction (units :*: units)
sqrt' :: Tolerance units => SurfaceFunction (units :*: units) -> SurfaceFunction units
quotient ::
  (Units.Quotient units1 units2 units3, Tolerance units2) =>
  SurfaceFunction units1 ->
  SurfaceFunction units2 ->
  SurfaceFunction units3
quotient' ::
  Tolerance units2 =>
  SurfaceFunction units1 ->
  SurfaceFunction units2 ->
  SurfaceFunction (units1 :/: units2)
sin :: SurfaceFunction Radians -> SurfaceFunction Unitless
cos :: SurfaceFunction Radians -> SurfaceFunction Unitless
