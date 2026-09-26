module OpenSolid.Curve.Nonzero (derivative) where

import OpenSolid.Curve (Curve)
import OpenSolid.Curve qualified as Curve
import OpenSolid.Nonzero (Nonzero (Nonzero))
import OpenSolid.VectorCurve (VectorCurve)

derivative :: Nonzero (Curve dimension units space) -> Nonzero (VectorCurve dimension units space)
derivative (Nonzero curve) = Nonzero (Curve.derivative curve)
