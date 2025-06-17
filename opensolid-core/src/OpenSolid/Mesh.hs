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
import OpenSolid.Bounded2d (Bounded2d)
import OpenSolid.Bounded2d qualified as Bounded2d
import OpenSolid.Bounded3d (Bounded3d)
import OpenSolid.Bounded3d qualified as Bounded3d
import OpenSolid.Bounds2d qualified as Bounds2d
import OpenSolid.Bounds3d qualified as Bounds3d
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Prelude
import OpenSolid.Vertex2d (Vertex2d)
import OpenSolid.Vertex3d (Vertex3d)

data Mesh vertex = Mesh (Array vertex) (List (Int, Int, Int))
  deriving (Eq, Show)

instance Vertex2d vertex (space @ units) => Bounded2d (Mesh vertex) (space @ units) where
  bounds mesh = Bounds2d.hullN (Array.toNonEmpty mesh.vertices)

instance Vertex3d vertex (space @ units) => Bounded3d (Mesh vertex) (space @ units) where
  bounds mesh = Bounds3d.hullN (Array.toNonEmpty mesh.vertices)

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
  let combinedVertices = Array.fromNonEmpty (NonEmpty.collect Array.toNonEmpty vertexArrays)
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
