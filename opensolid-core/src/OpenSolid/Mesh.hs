module OpenSolid.Mesh
  ( Mesh
  , indexed
  , vertices
  , faceIndices
  , faceVertices
  , map
  , concat
  , collect
  )
where

import OpenSolid.Array (Array)
import OpenSolid.Array qualified as Array
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Prelude

data Mesh vertex = Mesh
  { vertices :: Array vertex
  , faceIndices :: List (Int, Int, Int)
  }

indexed :: Array vertex -> List (Int, Int, Int) -> Mesh vertex
indexed = Mesh

faceVertices :: Mesh vertex -> List (vertex, vertex, vertex)
faceVertices (Mesh vertices faceIndices) = do
  let toVertices (i, j, k) = (Array.get i vertices, Array.get j vertices, Array.get k vertices)
  List.map toVertices faceIndices

map :: (a -> b) -> Mesh a -> Mesh b
map f (Mesh vertices faceIndices) = Mesh (Array.map f vertices) faceIndices

concat :: NonEmpty (Mesh a) -> Mesh a
concat meshes = do
  let vertexArrays = NonEmpty.map vertices meshes
  let faceIndexLists = NonEmpty.toList (NonEmpty.map faceIndices meshes)
  let arrayLengths = NonEmpty.toList (NonEmpty.map Array.length vertexArrays)
  let combinedVertices = Array.fromNonEmpty (NonEmpty.collect Array.toNonEmpty vertexArrays)
  let combinedFaceIndices = List.concat (offsetFaceIndices 0 arrayLengths faceIndexLists)
  Mesh combinedVertices combinedFaceIndices

collect :: (a -> Mesh b) -> NonEmpty a -> Mesh b
collect toMesh values = concat (NonEmpty.map toMesh values)

offsetFaceIndices :: Int -> List Int -> List (List (Int, Int, Int)) -> List (List (Int, Int, Int))
offsetFaceIndices offset (arraySize : remainingArraySizes) (faceIndices : remainingFaceIndices) =
  List.map (\(i, j, k) -> (i + offset, j + offset, k + offset)) faceIndices
    : offsetFaceIndices (offset + arraySize) remainingArraySizes remainingFaceIndices
offsetFaceIndices _ _ _ = []
