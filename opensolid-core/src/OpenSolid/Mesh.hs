module OpenSolid.Mesh
  ( Mesh
  , indexed
  , faceVertices
  , map
  , concat
  , collect
  )
where

import Data.Foldable1 (Foldable1)
import Data.Foldable1 qualified
import OpenSolid.Array (Array)
import OpenSolid.Array qualified as Array
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Prelude

data Mesh vertex = Mesh (Array vertex) (List (Int, Int, Int))
  deriving (Eq, Show)

indexed :: Array vertex -> List (Int, Int, Int) -> Mesh vertex
indexed = Mesh

instance HasField "vertices" (Mesh vertex) (Array vertex) where
  getField (Mesh vertices _) = vertices

instance HasField "faceIndices" (Mesh vertex) (List (Int, Int, Int)) where
  getField (Mesh _ faceIndices) = faceIndices

instance HasField "numVertices" (Mesh vertex) Int where
  getField = (.vertices.length)

instance HasField "numFaces" (Mesh vertex) Int where
  getField = (.faceIndices.length)

faceVertices :: Mesh vertex -> List (vertex, vertex, vertex)
faceVertices (Mesh vs fs) = do
  let toVertices (i, j, k) = (Array.get i vs, Array.get j vs, Array.get k vs)
  List.map toVertices fs

map :: (a -> b) -> Mesh a -> Mesh b
map f (Mesh vs fs) = Mesh (Array.map f vs) fs

concat :: NonEmpty (Mesh a) -> Mesh a
concat meshes = do
  let vertexArrays = NonEmpty.map (.vertices) meshes
  let faceIndexLists = NonEmpty.toList (NonEmpty.map (.faceIndices) meshes)
  let arrayLengths = NonEmpty.toList (NonEmpty.map (.length) vertexArrays)
  let combinedVertices = Array.fromList (List.collect Array.toList vertexArrays)
  let combinedFaceIndices = List.concat (offsetFaceIndices 0 arrayLengths faceIndexLists)
  Mesh combinedVertices combinedFaceIndices

collect :: Foldable1 list => (a -> Mesh b) -> list a -> Mesh b
collect toMesh values = concat (NonEmpty.map toMesh (Data.Foldable1.toNonEmpty values))

offsetFaceIndices :: Int -> List Int -> List (List (Int, Int, Int)) -> List (List (Int, Int, Int))
offsetFaceIndices _ [] _ = []
offsetFaceIndices _ _ [] = []
offsetFaceIndices offset (NonEmpty vertexArraySizes) (NonEmpty faceIndicesLists) =
  List.map (addOffset offset) faceIndicesLists.first
    : offsetFaceIndices
      (offset + vertexArraySizes.first)
      vertexArraySizes.rest
      faceIndicesLists.rest

addOffset :: Int -> (Int, Int, Int) -> (Int, Int, Int)
addOffset offset (i, j, k) = (i + offset, j + offset, k + offset)
