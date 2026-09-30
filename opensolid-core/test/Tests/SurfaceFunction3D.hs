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
  let pLeft = function @ UvPoint uLeft vMid
  let pRight = function @ UvPoint uRight vMid
  let pLower = function @ UvPoint uMid vLower
  let pUpper = function @ UvPoint uMid vUpper
  let duNumeric = (pRight - pLeft) / (2.0 * delta)
  let dvNumeric = (pUpper - pLower) / (2.0 * delta)
  let (duAnalytic, dvAnalytic) = SurfaceFunction3D.partialDerivatives function @ uvPoint
  Tolerance.using (Length.meters 1e-6) $
    Test.all
      [ Test.expect (duNumeric ~= duAnalytic) & Test.log {duNumeric, duAnalytic}
      , Test.expect (dvNumeric ~= dvAnalytic) & Test.log {dvNumeric, dvAnalytic}
      ]
