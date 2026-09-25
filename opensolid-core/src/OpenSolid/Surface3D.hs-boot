module OpenSolid.Surface3D
  ( Surface3D
  , Pole (Pole)
  , Edge (Edge)
  , IsDegenerate
  , parametric
  )
where

import OpenSolid.Curve3D (Curve3D)
import OpenSolid.Point3D (Point3D)
import OpenSolid.Prelude
import OpenSolid.Space qualified as Space
import {-# SOURCE #-} OpenSolid.SurfaceFunction3D (SurfaceFunction3D)
import OpenSolid.UvCurve (UvCurve)
import OpenSolid.UvRegion (UvRegion)

type role Surface3D nominal

type Surface3D :: Type -> Type
data Surface3D space

data Pole space = Pole UvCurve (Point3D space)

instance Show (Pole space)

instance Space.Coercion (Pole space1) (Pole space2)

data Edge space = Edge UvCurve (Curve3D space)

instance Show (Edge space)

instance Space.Coercion (Edge space1) (Edge space2)

data IsDegenerate

parametric :: Tolerance Meters => SurfaceFunction3D space -> UvRegion -> Result IsDegenerate (Surface3D space)
