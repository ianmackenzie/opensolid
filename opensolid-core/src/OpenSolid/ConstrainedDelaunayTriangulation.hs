{-# LANGUAGE NoFieldSelectors #-}

module OpenSolid.ConstrainedDelaunayTriangulation
  ( Option
  , steinerPoints
  , maxRefinementPoints
  , unsafe
  )
where

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
import System.IO.Unsafe qualified

data Option (coordinateSystem :: CoordinateSystem) where
  SteinerPoints :: List (Point2d (space @ units)) -> Option (space @ units)
  MaxRefinementPoints :: Int -> Option (space @ units)

data Options (coordinateSystem :: CoordinateSystem) where
  Options ::
    { steinerPoints :: List (Point2d (space @ units))
    , maxRefinementPoints :: Int
    } ->
    Options (space @ units)

defaultOptions :: Options (space @ units)
defaultOptions =
  Options
    { steinerPoints = []
    , maxRefinementPoints = 0
    }

setOption :: Options (space @ units) -> Option (space @ units) -> Options (space @ units)
setOption options option = case option of
  SteinerPoints points -> options{steinerPoints = points}
  MaxRefinementPoints count -> options{maxRefinementPoints = count}

steinerPoints :: List (Point2d (space @ units)) -> Option (space @ units)
steinerPoints = SteinerPoints

maxRefinementPoints :: Int -> Option (space @ units)
maxRefinementPoints = MaxRefinementPoints

unsafe ::
  List (Option (space @ units)) ->
  NonEmpty (NonEmpty (Point2d (space @ units))) ->
  Mesh (Point2d (space @ units))
unsafe options loops = do
  let Options
        { steinerPoints = givenSteinerPoints
        , maxRefinementPoints = givenMaxRefinementPoints
        } = List.foldl setOption defaultOptions options
  let loopList = NonEmpty.toList (NonEmpty.map NonEmpty.toList loops)
  let inputVertexData =
        List.foldr collectVertexData (collectVertexData givenSteinerPoints []) loopList
  let inputEdgeData = collectEdgeIndices loopList 0 []
  let numHoles = NonEmpty.length loops - 1
  let numBoundaryVertices = Int.sumOf List.length loopList
  let numBoundaryEdges = numBoundaryVertices
  let numInputVertices = numBoundaryVertices + List.length givenSteinerPoints
  let maxNumOutputVertices = numInputVertices + givenMaxRefinementPoints
  let maxNumOutputFaces = 2 * maxNumOutputVertices - numBoundaryEdges + 2 * numHoles - 2
  System.IO.Unsafe.unsafeDupablePerformIO IO.do
    Foreign.Marshal.Array.withArray inputVertexData $ \inputVerticesPtr ->
      Foreign.Marshal.Array.withArray inputEdgeData $ \inputEdgesPtr ->
        Foreign.Marshal.alloca $ \numOutputVerticesPtr ->
          Foreign.Marshal.Array.allocaArray (2 * maxNumOutputVertices) $ \outputVerticesPtr ->
            Foreign.Marshal.alloca $ \numOutputFacesPtr ->
              Foreign.Marshal.Array.allocaArray (3 * maxNumOutputFaces) $ \outputFacesPtr -> IO.do
                opensolid_polygon2d_triangulate
                  (Int.toCSize numInputVertices)
                  inputVerticesPtr
                  (Int.toCSize numBoundaryEdges)
                  inputEdgesPtr
                  (Int.toCSize givenMaxRefinementPoints)
                  numOutputVerticesPtr
                  outputVerticesPtr
                  numOutputFacesPtr
                  outputFacesPtr
                numOutputVertices <- IO.map Int.fromCSize (Foreign.peek numOutputVerticesPtr)
                numOutputFaces <- IO.map Int.fromCSize (Foreign.peek numOutputFacesPtr)
                outputVertexData <-
                  Foreign.Marshal.peekArray (2 * numOutputVertices) outputVerticesPtr
                outputFaceData <-
                  Foreign.Marshal.peekArray (3 * numOutputFaces) outputFacesPtr
                let outputFaceIndices = collectFaceIndices outputFaceData
                case collectVertices outputVertexData of
                  NonEmpty outputVertices ->
                    IO.succeed (Mesh.indexed (Array.fromNonEmpty outputVertices) outputFaceIndices)
                  [] -> internalError "Constrained Delaunay triangulation failed"

collectVertexData :: List (Point2d (space @ units)) -> List (Qty units) -> List (Qty units)
collectVertexData loop accumulated = List.foldr prependVertex accumulated loop

prependVertex :: Point2d (space @ units) -> List (Qty units) -> List (Qty units)
prependVertex (Point2d x y) accumulated = x : y : accumulated

collectEdgeIndices :: List (List (Point2d (space @ units))) -> Int -> List CSize -> List CSize
collectEdgeIndices loops startIndex accumulated = case loops of
  [] -> accumulated
  first : rest -> do
    let firstLength = List.length first
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
    CSize -> -- Number of input vertices (edge vertices + Steiner points)
    Foreign.Ptr (Qty units) -> -- Vertex data as [x0, y0, x1, y1, x2, y2, ...]
    CSize -> -- Total number of edges
    Foreign.Ptr CSize -> -- Edge data as vertex index pairs [ start0, end0, start1, end1, ...]
    CSize -> -- Max number of refinement points
    Foreign.Ptr CSize -> -- Number of points in triangulation
    Foreign.Ptr (Qty units) -> -- Triangulation point data as [x0, y0, x1, y1, x2, y2, ...]
    Foreign.Ptr CSize -> -- Number of triangles in triangulation
    Foreign.Ptr CSize -> -- Triangle data as point index triples [i0, j0, k0, i1, j1, k1, ...]
    IO ()

collectVertices :: List (Qty units) -> List (Point2d (space @ units))
collectVertices vertexData = case vertexData of
  x : y : rest -> Point2d x y : collectVertices rest
  _ -> []

collectFaceIndices :: List CSize -> List (Int, Int, Int)
collectFaceIndices faceData = case faceData of
  i : j : k : rest -> (Int.fromCSize i, Int.fromCSize j, Int.fromCSize k) : collectFaceIndices rest
  _ -> []
