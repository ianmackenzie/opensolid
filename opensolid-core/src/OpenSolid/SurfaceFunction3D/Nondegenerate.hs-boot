module OpenSolid.SurfaceFunction3D.Nondegenerate
  ( pointAt
  , pointOn
  , range
  , partialDerivatives
  , partialDerivativesAt
  , partialDerivativeRanges
  , secondPartialDerivatives
  , secondPartialDerivativesAt
  , secondPartialDerivativeRanges
  , normalDirectionAt
  , normalDirectionRange
  , degenerateLeft
  , degenerateRight
  , degenerateBottom
  , degenerateTop
  )
where

import OpenSolid.Bounds3D (Bounds3D)
import OpenSolid.Direction3D (Direction3D)
import OpenSolid.DirectionBounds3D (DirectionBounds3D)
import OpenSolid.Nondegenerate (Nondegenerate)
import OpenSolid.Point3D (Point3D)
import OpenSolid.Prelude
import {-# SOURCE #-} OpenSolid.SurfaceFunction3D (SurfaceFunction3D)
import OpenSolid.UvBounds (UvBounds)
import OpenSolid.UvPoint (UvPoint)
import OpenSolid.Vector3D (Vector3D)
import OpenSolid.VectorBounds3D (VectorBounds3D)
import OpenSolid.VectorSurfaceFunction3D (VectorSurfaceFunction3D)

pointAt :: UvPoint -> Nondegenerate (SurfaceFunction3D space) -> Point3D space
pointOn :: Nondegenerate (SurfaceFunction3D space) -> UvPoint -> Point3D space
range :: UvBounds -> Nondegenerate (SurfaceFunction3D space) -> Bounds3D space
partialDerivatives ::
  Nondegenerate (SurfaceFunction3D space) ->
  ( Nondegenerate (VectorSurfaceFunction3D Meters space)
  , Nondegenerate (VectorSurfaceFunction3D Meters space)
  )
partialDerivativesAt ::
  UvPoint ->
  Nondegenerate (SurfaceFunction3D space) ->
  (Vector3D Meters space, Vector3D Meters space)
partialDerivativeRanges ::
  UvBounds ->
  Nondegenerate (SurfaceFunction3D space) ->
  (VectorBounds3D Meters space, VectorBounds3D Meters space)
secondPartialDerivatives ::
  Nondegenerate (SurfaceFunction3D space) ->
  ( VectorSurfaceFunction3D Meters space
  , VectorSurfaceFunction3D Meters space
  , VectorSurfaceFunction3D Meters space
  )
secondPartialDerivativesAt ::
  UvPoint ->
  Nondegenerate (SurfaceFunction3D space) ->
  (Vector3D Meters space, Vector3D Meters space, Vector3D Meters space)
secondPartialDerivativeRanges ::
  UvBounds ->
  Nondegenerate (SurfaceFunction3D space) ->
  (VectorBounds3D Meters space, VectorBounds3D Meters space, VectorBounds3D Meters space)
normalDirectionAt ::
  UvPoint ->
  Nondegenerate (SurfaceFunction3D space) ->
  Direction3D space
normalDirectionRange ::
  UvBounds ->
  Nondegenerate (SurfaceFunction3D space) ->
  DirectionBounds3D space
degenerateLeft :: Nondegenerate (SurfaceFunction3D space) -> Bool
degenerateRight :: Nondegenerate (SurfaceFunction3D space) -> Bool
degenerateBottom :: Nondegenerate (SurfaceFunction3D space) -> Bool
degenerateTop :: Nondegenerate (SurfaceFunction3D space) -> Bool
