{-# LANGUAGE NoFieldSelectors #-}

module OpenSolid.CDT (unsafe) where

import Foreign qualified
import Foreign.Marshal qualified
import Foreign.Marshal.Array qualified
import OpenSolid.Array qualified as Array
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
  Mesh vertex
unsafe boundaryLoops steinerVertices = do
  let boundaryVertices = NonEmpty.concat boundaryLoops
  let inputVertices = NonEmpty.extend boundaryVertices steinerVertices
  let prependCoordinates (Vertex2d (Point2d x y)) accumulated = x : y : accumulated
  let inputPointCoordinates = NonEmpty.foldr prependCoordinates [] inputVertices
  let inputEdgeIndices = collectEdgeIndices (NonEmpty.toList boundaryLoops) 0 []
  let numHoles = NonEmpty.length boundaryLoops - 1
  let numBoundaryEdges = NonEmpty.length boundaryVertices
  let numInputVertices = NonEmpty.length inputVertices
  let maxNumOutputVertices = numInputVertices
  let maxNumOutputFaces = 2 * maxNumOutputVertices - numBoundaryEdges + 2 * numHoles - 2
  System.IO.Unsafe.unsafePerformIO do
    Foreign.Marshal.Array.withArray inputPointCoordinates $ \inputPointData ->
      Foreign.Marshal.Array.withArray inputEdgeIndices $ \inputEdgeData ->
        Foreign.Marshal.alloca $ \numOutputTrianglesPtr ->
          Foreign.Marshal.Array.allocaArray (3 * maxNumOutputFaces) $ \triangleData -> do
            opensolid_cdt
              numInputVertices
              inputPointData
              numBoundaryEdges
              inputEdgeData
              numOutputTrianglesPtr
              triangleData
            numOutputFaces <- Foreign.peek numOutputTrianglesPtr
            outputFaceIndices <- Foreign.Marshal.peekArray (3 * numOutputFaces) triangleData
            let faceIndices = collectFaceIndices outputFaceIndices
            let meshVertices = Array.fromNonEmpty inputVertices
            return (Mesh.indexed meshVertices faceIndices)

collectEdgeIndices :: List (NonEmpty vertex) -> Int -> List Int -> List Int
collectEdgeIndices loops startIndex accumulated = case loops of
  [] -> accumulated
  first : rest -> do
    let firstLength = NonEmpty.length first
    collectLoopEdgeIndices startIndex firstLength accumulated
      |> collectEdgeIndices rest (startIndex + firstLength)

collectLoopEdgeIndices :: Int -> Int -> List Int -> List Int
collectLoopEdgeIndices startIndex loopLength accumulated = do
  let addEdge i acc = do
        let edgeStart = startIndex + i
        let edgeEnd = startIndex + (i + 1) `mod` loopLength
        edgeStart : edgeEnd : acc
  List.foldr addEdge accumulated [0 .. loopLength - 1]

foreign import ccall safe "opensolid_cdt"
  opensolid_cdt ::
    Int -> -- input_point_count
    Foreign.Ptr (Quantity units) -> -- input_point_data
    Int -> -- input_edge_count
    Foreign.Ptr Int -> -- input_edge_data
    Foreign.Ptr Int -> -- output_triangle_count
    Foreign.Ptr Int -> -- output_triangle_data
    IO ()

collectFaceIndices :: List Int -> List (Int, Int, Int)
collectFaceIndices (i : j : k : rest) = (i, j, k) : collectFaceIndices rest
collectFaceIndices _ = []
