module Tests.SurfaceFunction3D (partialDerivativesAreConsistent) where

import OpenSolid.Length qualified as Length
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Prelude
import OpenSolid.SurfaceFunction3D (SurfaceFunction3D)
import OpenSolid.SurfaceFunction3D qualified as SurfaceFunction3D
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.UvPoint (UvPoint, data UvPoint)
import OpenSolid.UvPoint qualified as UvPoint
import Test (Expectation)
import Test qualified

partialDerivativesAreConsistent :: SurfaceFunction3D space -> Expectation
partialDerivativesAreConsistent function = do
  let consistentAt uvPoint = partialDerivativesAreConsistentAt uvPoint function
  Test.all (List.map consistentAt (NonEmpty.toList UvPoint.interiorSamples))

partialDerivativesAreConsistentAt :: UvPoint -> SurfaceFunction3D space -> Expectation
partialDerivativesAreConsistentAt uvPoint function = do
  let UvPoint uMid vMid = uvPoint
  let delta = 1e-6
  let uLeft = uMid - delta
  let uRight = uMid + delta
  let vLower = vMid - delta
  let vUpper = vMid + delta
  let pLeft = SurfaceFunction3D.pointAt (UvPoint uLeft vMid) function
  let pRight = SurfaceFunction3D.pointAt (UvPoint uRight vMid) function
  let pLower = SurfaceFunction3D.pointAt (UvPoint uMid vLower) function
  let pUpper = SurfaceFunction3D.pointAt (UvPoint uMid vUpper) function
  let duNumeric = (pRight - pLeft) / (2.0 * delta)
  let dvNumeric = (pUpper - pLower) / (2.0 * delta)
  let (duAnalytic, dvAnalytic) = SurfaceFunction3D.partialDerivativesAt uvPoint function
  Tolerance.using (Length.meters 1e-6) $
    Test.all
      [ Test.expect (duNumeric ~= duAnalytic)
          & Test.output "duNumeric" duNumeric
          & Test.output "duAnalytic" duAnalytic
      , Test.expect (dvNumeric ~= dvAnalytic)
          & Test.output "dvNumeric" dvNumeric
          & Test.output "dvAnalytic" dvAnalytic
      ]
