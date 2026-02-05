module OpenSolid.Mesh
  ( Mesh
  , empty
  , vertices
  , faceIndices
  , numVertices
  , numFaces
  , indexed
  , faceVertices
  , map
  , concat
  , combine
  , grid
  , indexedGrid
  )
where

import OpenSolid.Array (Array)
import OpenSolid.Array qualified as Array
import OpenSolid.Int qualified as Int
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Prelude
import OpenSolid.UvPoint (UvPoint, pattern UvPoint)

data Mesh vertex = Mesh
  { vertices :: Array vertex
  , faceIndices :: List (Int, Int, Int)
  }
  deriving (Eq, Show)

indexed :: Array vertex -> List (Int, Int, Int) -> Mesh vertex
indexed = Mesh

vertices :: Mesh vertex -> Array vertex
vertices mesh = mesh.vertices

faceIndices :: Mesh vertex -> List (Int, Int, Int)
faceIndices mesh = mesh.faceIndices

numVertices :: Mesh vertex -> Int
numVertices mesh = Array.length (vertices mesh)

numFaces :: Mesh vertex -> Int
numFaces mesh = List.length (faceIndices mesh)

empty :: Mesh vertex
empty = Mesh Array.empty []

faceVertices :: Mesh vertex -> List (vertex, vertex, vertex)
faceVertices (Mesh vs fs) = do
  let toVertices (i, j, k) = (Array.get i vs, Array.get j vs, Array.get k vs)
  List.map toVertices fs

map :: (a -> b) -> Mesh a -> Mesh b
map f (Mesh vs fs) = Mesh (Array.map f vs) fs

concat :: List (Mesh a) -> Mesh a
concat meshes = do
  let vertexArrays = List.map vertices meshes
  let faceIndexLists = List.map faceIndices meshes
  let arrayLengths = List.map Array.length vertexArrays
  let combinedVertices = Array.fromList (List.combine Array.toList vertexArrays)
  let combinedFaceIndices = List.concat (offsetFaceIndices 0 arrayLengths faceIndexLists)
  Mesh combinedVertices combinedFaceIndices

combine :: (a -> Mesh b) -> List a -> Mesh b
combine toMesh values = concat (List.map toMesh values)

offsetFaceIndices :: Int -> List Int -> List (List (Int, Int, Int)) -> List (List (Int, Int, Int))
offsetFaceIndices _ [] _ = []
offsetFaceIndices _ _ [] = []
offsetFaceIndices offset (NonEmpty vertexArraySizes) (NonEmpty faceIndicesLists) =
  List.map (addOffset offset) (NonEmpty.first faceIndicesLists)
    : offsetFaceIndices
      (offset + NonEmpty.first vertexArraySizes)
      (NonEmpty.rest vertexArraySizes)
      (NonEmpty.rest faceIndicesLists)

addOffset :: Int -> (Int, Int, Int) -> (Int, Int, Int)
addOffset offset (i, j, k) = (i + offset, j + offset, k + offset)

grid :: Int -> Int -> (UvPoint -> vertex) -> Mesh vertex
grid uSteps vSteps function =
  indexedGrid uSteps vSteps (toIndexedFunction uSteps vSteps function)

toIndexedFunction :: Int -> Int -> (UvPoint -> vertex) -> Int -> Int -> vertex
toIndexedFunction uSteps vSteps function uIndex vIndex =
  function (UvPoint (Int.ratio uIndex uSteps) (Int.ratio vIndex vSteps))

indexedGrid :: Int -> Int -> (Int -> Int -> vertex) -> Mesh vertex
indexedGrid uSteps vSteps function =
  gridImpl uSteps vSteps (uSteps + 1) (vSteps + 1) function

gridImpl :: Int -> Int -> Int -> Int -> (Int -> Int -> vertex) -> Mesh vertex
gridImpl uSteps vSteps uVertices vVertices function =
  if uVertices <= 1 || vVertices <= 1
    then empty
    else do
      let gridVertices =
            Array.initialize (uVertices * vVertices) do
              \i -> function (i % uVertices) (i // uVertices)
      let gridFaceIndices =
            buildGridFaceIndices uSteps uVertices vVertices (uSteps - 1) (vSteps - 1) []
      Mesh gridVertices gridFaceIndices

buildGridFaceIndices ::
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  List (Int, Int, Int) ->
  List (Int, Int, Int)
buildGridFaceIndices uSteps uVertices vVertices uIndex0 vIndex0 accumulatedIndices = do
  let rowStart0 = uVertices * vIndex0
  let rowStart1 = uVertices * ((vIndex0 + 1) % vVertices)
  let uIndex1 = (uIndex0 + 1) % uVertices
  let index00 = rowStart0 + uIndex0
  let index10 = rowStart0 + uIndex1
  let index01 = rowStart1 + uIndex0
  let index11 = rowStart1 + uIndex1
  let lowerFaceIndices = (index00, index10, index11)
  let upperFaceIndices = (index00, index11, index01)
  let updatedIndices = lowerFaceIndices : upperFaceIndices : accumulatedIndices
  if
    | uIndex0 > 0 ->
        buildGridFaceIndices uSteps uVertices vVertices (uIndex0 - 1) vIndex0 updatedIndices
    | vIndex0 > 0 ->
        buildGridFaceIndices uSteps uVertices vVertices (uSteps - 1) (vIndex0 - 1) updatedIndices
    | otherwise -> updatedIndices
