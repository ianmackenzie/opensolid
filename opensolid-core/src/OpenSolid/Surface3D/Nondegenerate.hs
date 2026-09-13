module OpenSolid.Surface3D.Nondegenerate
  ( function
  , domain
  , outerBoundary
  , outerLoop
  , innerBoundaries
  , innerLoops
  , boundaries
  , boundaryLoops
  , boundaryCurves
  , vertices
  , edges
  , findPoint
  )
where

import OpenSolid.Bag3D (Bag3D)
import OpenSolid.Nondegenerate (Nondegenerate (Nondegenerate))
import OpenSolid.Point3D (Point3D)
import OpenSolid.Prelude
import OpenSolid.Set3D (Set3D)
import OpenSolid.Surface3D (Surface3D)
import OpenSolid.Surface3D qualified as Surface3D
import {-# SOURCE #-} OpenSolid.Surface3D.Nondegenerate.FindPoint qualified as Surface3D.Nondegenerate.FindPoint
import OpenSolid.SurfaceCurve3D (SurfaceCurve3D)
import OpenSolid.SurfaceFunction3D (SurfaceFunction3D)
import OpenSolid.SurfacePoint3D (SurfacePoint3D)
import OpenSolid.UvRegion (UvRegion)

function :: Nondegenerate (Surface3D space) -> Nondegenerate (SurfaceFunction3D space)
function (Nondegenerate surface) = Nondegenerate (Surface3D.function surface)

domain :: Nondegenerate (Surface3D space) -> UvRegion
domain (Nondegenerate surface) = Surface3D.domain surface

outerBoundary :: Nondegenerate (Surface3D space) -> Surface3D.Boundary space
outerBoundary (Nondegenerate surface) = Surface3D.outerBoundary surface

outerLoop :: Nondegenerate (Surface3D space) -> NonEmpty (SurfaceCurve3D space)
outerLoop (Nondegenerate surface) = Surface3D.outerLoop surface

innerBoundaries :: Nondegenerate (Surface3D space) -> Bag3D space (Surface3D.Boundary space)
innerBoundaries (Nondegenerate surface) = Surface3D.innerBoundaries surface

innerLoops :: Nondegenerate (Surface3D space) -> List (NonEmpty (SurfaceCurve3D space))
innerLoops (Nondegenerate surface) = Surface3D.innerLoops surface

boundaries :: Nondegenerate (Surface3D space) -> Set3D space (Surface3D.Boundary space)
boundaries (Nondegenerate surface) = Surface3D.boundaries surface

boundaryLoops :: Nondegenerate (Surface3D space) -> NonEmpty (NonEmpty (SurfaceCurve3D space))
boundaryLoops (Nondegenerate surface) = Surface3D.boundaryLoops surface

boundaryCurves :: Nondegenerate (Surface3D space) -> Set3D space (SurfaceCurve3D space)
boundaryCurves (Nondegenerate surface) = Surface3D.boundaryCurves surface

vertices ::
  Tolerance Meters =>
  Nondegenerate (Surface3D space) ->
  Bag3D space (SurfacePoint3D space)
vertices (Nondegenerate surface) = Surface3D.vertices surface

edges ::
  Tolerance Meters =>
  Nondegenerate (Surface3D space) ->
  Bag3D space (Nondegenerate (SurfaceCurve3D space))
edges (Nondegenerate surface) = Surface3D.edges surface

findPoint ::
  Tolerance Meters =>
  Point3D space ->
  Nondegenerate (Surface3D space) ->
  List (SurfacePoint3D space)
findPoint = Surface3D.Nondegenerate.FindPoint.findPoint
