module OpenSolid.Polygon2d
  ( Polygon2d
  , outerLoop
  , innerLoops
  , singleLoop
  , withHoles
  , triangulate
  )
where

import Foreign qualified
import Foreign.C.Types (CSize (CSize))
import Foreign.Marshal.Array qualified
import OpenSolid.Array qualified as Array
import OpenSolid.Debug qualified as Debug
import OpenSolid.IO qualified as IO
import OpenSolid.Int qualified as Int
import OpenSolid.List qualified as List
import OpenSolid.Mesh (Mesh)
import OpenSolid.Mesh qualified as Mesh
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Point2d (Point2d (Point2d))
import OpenSolid.Prelude
import OpenSolid.Qty qualified as Qty
import OpenSolid.Triangle2d (Triangle2d (Triangle2d))
import OpenSolid.Triangle2d qualified as Triangle2d
import OpenSolid.Vertex2d (Vertex2d, pattern Vertex2d)
import System.IO.Unsafe (unsafeDupablePerformIO)

data Polygon2d vertex = Polygon2d
  { outerLoop :: NonEmpty vertex
  , innerLoops :: List (NonEmpty vertex)
  }

singleLoop :: Vertex2d vertex (space @ units) => NonEmpty vertex -> Polygon2d vertex
singleLoop givenOuterLoop = withHoles [] givenOuterLoop

withHoles ::
  Vertex2d vertex (space @ units) =>
  List (NonEmpty vertex) ->
  NonEmpty vertex ->
  Polygon2d vertex
withHoles givenInnerLoops givenOuterLoop =
  Polygon2d
    { outerLoop = makeLoop Positive givenOuterLoop
    , innerLoops = List.map (makeLoop Negative) givenInnerLoops
    }

makeLoop :: Vertex2d vertex (space @ units) => Sign -> NonEmpty vertex -> NonEmpty vertex
makeLoop desiredSign vertices =
  if Qty.sign (counterclockwiseArea' vertices) == desiredSign
    then vertices
    else NonEmpty.reverse vertices

counterclockwiseArea' :: Vertex2d vertex (space @ units) => NonEmpty vertex -> Qty (units :*: units)
counterclockwiseArea' (v0 :| vs) = do
  let triangleSignedArea' v1 v2 = Triangle2d.signedArea' (Triangle2d v0 v1 v2)
  Qty.sum (List.successive triangleSignedArea' vs)

triangulate :: Vertex2d vertex (space @ units) => Polygon2d vertex -> Mesh vertex
triangulate (Polygon2d outerLoop innerLoops) = do
  let prependCoordinates (Vertex2d (Point2d x y)) acc = x : y : acc
  let allVertices = NonEmpty.concat (outerLoop :| innerLoops)
  let numVertices = NonEmpty.length allVertices
  let vertexCoordinates = NonEmpty.foldr prependCoordinates [] allVertices
  let holeIndices = collectHoleIndices (NonEmpty.length outerLoop) innerLoops
  let numHoles = List.length holeIndices
  let numFaces = numVertices + 2 * numHoles - 2
  let faceIndices = unsafeDupablePerformIO $ IO.do
        Foreign.Marshal.Array.withArray vertexCoordinates $ \vertexPtr ->
          Foreign.Marshal.Array.withArray holeIndices $ \holesPtr ->
            Foreign.Marshal.Array.allocaArray (3 * numFaces) $ \faceIndicesPtr -> IO.do
              let numFacesReturned =
                    opensolid_polygon2d_triangulate
                      (Int.toCSize numVertices)
                      vertexPtr
                      (Int.toCSize numHoles)
                      holesPtr
                      faceIndicesPtr
              if Int.fromCSize numFacesReturned == numFaces
                then IO.do
                  rawIndices <- Foreign.Marshal.Array.peekArray (3 * numFaces) faceIndicesPtr
                  IO.succeed (collectTriangleIndices rawIndices)
                else IO.do
                  Debug.assert False
                  IO.succeed []
  Mesh.indexed (Array.fromNonEmpty allVertices) faceIndices

collectHoleIndices :: Vertex2d vertex (space @ units) => Int -> List (NonEmpty vertex) -> List CSize
collectHoleIndices _ [] = []
collectHoleIndices current (first : rest) =
  Int.toCSize current : collectHoleIndices (current + NonEmpty.length first) rest

collectTriangleIndices :: List CSize -> List (Int, Int, Int)
collectTriangleIndices rawIndices = case rawIndices of
  i : j : k : rest ->
    (Int.fromCSize i, Int.fromCSize j, Int.fromCSize k) : collectTriangleIndices rest
  _ -> []

foreign import ccall unsafe "opensolid_polygon2d_triangulate"
  opensolid_polygon2d_triangulate ::
    CSize -> -- Vertex count
    Foreign.Ptr (Qty units) -> -- Vertex data
    CSize -> -- Hole count (including Steiner points)
    Foreign.Ptr CSize -> -- Hole start indices
    Foreign.Ptr CSize -> -- Output triangle indices
    CSize
