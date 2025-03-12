{-# LANGUAGE NoFieldSelectors #-}

module OpenSolid.ConstrainedDelaunayTriangulation (unsafe) where

import Foreign qualified
import Foreign.C.Types (CSize (CSize))
import Foreign.Marshal qualified
import Foreign.Marshal.Array qualified
import OpenSolid.Array qualified as Array
import OpenSolid.IO qualified as IO
import OpenSolid.Int qualified as Int
import OpenSolid.List qualified as List
import OpenSolid.Mesh (Mesh)
import OpenSolid.Mesh qualified as Mesh
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Point2d (Point2d (Point2d))
import OpenSolid.Prelude
import OpenSolid.Vertex2d (Vertex2d, pattern Vertex2d)
import System.IO.Unsafe qualified

unsafe ::
  Vertex2d vertex (space @ units) =>
  NonEmpty (NonEmpty vertex) ->
  List vertex ->
  Maybe (Int, (Point2d (space @ units) -> vertex)) ->
  Mesh vertex
unsafe boundaryLoops steinerVertices refinement = do
  let boundaryVertices = NonEmpty.concat boundaryLoops
  let inputVertices = NonEmpty.extend boundaryVertices steinerVertices
  let prependCoordinates (Vertex2d (Point2d x y)) accumulated = x : y : accumulated
  let inputPointCoordinates = NonEmpty.foldr prependCoordinates [] inputVertices
  let inputEdgeIndices = collectEdgeIndices (NonEmpty.toList boundaryLoops) 0 []
  let numHoles = NonEmpty.length boundaryLoops - 1
  let numBoundaryEdges = NonEmpty.length boundaryVertices
  let numInputVertices = NonEmpty.length inputVertices
  let maxRefinementPoints = case refinement of
        Nothing -> 0
        Just (givenMaxRefinementPoints, _) -> givenMaxRefinementPoints
  let maxNumOutputVertices = numInputVertices + maxRefinementPoints
  let maxNumOutputFaces = 2 * maxNumOutputVertices - numBoundaryEdges + 2 * numHoles - 2
  System.IO.Unsafe.unsafePerformIO IO.do
    Foreign.Marshal.Array.withArray inputPointCoordinates $ \inputPointData ->
      Foreign.Marshal.Array.withArray inputEdgeIndices $ \inputEdgeData ->
        Foreign.Marshal.alloca $ \numOutputRefinementVerticesPtr ->
          Foreign.Marshal.Array.allocaArray (2 * maxRefinementPoints) $ \refinementPointData ->
            Foreign.Marshal.alloca $ \numOutputTrianglesPtr ->
              Foreign.Marshal.Array.allocaArray (3 * maxNumOutputFaces) $ \triangleData -> IO.do
                opensolid_polygon2d_triangulate
                  (Int.toCSize numInputVertices)
                  inputPointData
                  (Int.toCSize numBoundaryEdges)
                  inputEdgeData
                  (Int.toCSize maxRefinementPoints)
                  numOutputRefinementVerticesPtr
                  refinementPointData
                  numOutputTrianglesPtr
                  triangleData
                numRefinementVertices <-
                  IO.map Int.fromCSize (Foreign.peek numOutputRefinementVerticesPtr)
                numOutputFaces <-
                  IO.map Int.fromCSize (Foreign.peek numOutputTrianglesPtr)
                outputRefinementPointCoordinates <-
                  Foreign.Marshal.peekArray (2 * numRefinementVertices) refinementPointData
                outputFaceIndices <-
                  Foreign.Marshal.peekArray (3 * numOutputFaces) triangleData
                let faceIndices = collectFaceIndices outputFaceIndices
                let refinementVertices = case refinement of
                      Nothing -> []
                      Just (_, toVertex) ->
                        collectRefinementVertices toVertex outputRefinementPointCoordinates
                let meshVertices = Array.fromNonEmpty (NonEmpty.extend inputVertices refinementVertices)
                IO.succeed (Mesh.indexed meshVertices faceIndices)

collectEdgeIndices :: List (NonEmpty vertex) -> Int -> List CSize -> List CSize
collectEdgeIndices loops startIndex accumulated = case loops of
  [] -> accumulated
  first : rest -> do
    let firstLength = NonEmpty.length first
    collectLoopEdgeIndices startIndex firstLength accumulated
      |> collectEdgeIndices rest (startIndex + firstLength)

collectLoopEdgeIndices :: Int -> Int -> List CSize -> List CSize
collectLoopEdgeIndices startIndex loopLength accumulated = do
  let addEdge i acc = do
        let edgeStart = startIndex + i
        let edgeEnd = startIndex + (i + 1) % loopLength
        Int.toCSize edgeStart : Int.toCSize edgeEnd : acc
  List.foldr addEdge accumulated [0 .. loopLength - 1]

foreign import ccall unsafe "opensolid_cdt"
  opensolid_polygon2d_triangulate ::
    CSize -> -- input_point_count
    Foreign.Ptr (Qty units) -> -- input_point_data
    CSize -> -- input_edge_count
    Foreign.Ptr CSize -> -- input_edge_data
    CSize -> -- input_max_refinement_point_count
    Foreign.Ptr CSize -> -- output_refinement_point_count
    Foreign.Ptr (Qty units) -> -- output_refinement_point_data
    Foreign.Ptr CSize -> -- output_triangle_count
    Foreign.Ptr CSize -> -- output_triangle_data
    IO ()

collectRefinementVertices :: (Point2d (space @ units) -> vertex) -> List (Qty units) -> List vertex
collectRefinementVertices toVertex vertexData = case vertexData of
  x : y : rest -> toVertex (Point2d x y) : collectRefinementVertices toVertex rest
  _ -> []

collectFaceIndices :: List CSize -> List (Int, Int, Int)
collectFaceIndices faceData = case faceData of
  i : j : k : rest -> (Int.fromCSize i, Int.fromCSize j, Int.fromCSize k) : collectFaceIndices rest
  _ -> []
