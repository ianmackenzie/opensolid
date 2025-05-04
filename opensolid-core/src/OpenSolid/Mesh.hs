{-# LANGUAGE NoFieldSelectors #-}

module OpenSolid.Mesh
  ( Mesh
  , Constraint
  , maxError
  , maxSize
  , Constraints (..)
  , constraints
  , indexed
  , vertices
  , faceIndices
  , faceVertices
  , map
  , concat
  , collect
  )
where

import Data.Foldable1 (Foldable1)
import Data.Foldable1 qualified
import Data.Proxy (Proxy (Proxy))
import OpenSolid.Array (Array)
import OpenSolid.Array qualified as Array
import OpenSolid.Bounded2d (Bounded2d)
import OpenSolid.Bounded2d qualified as Bounded2d
import OpenSolid.Bounded3d (Bounded3d)
import OpenSolid.Bounded3d qualified as Bounded3d
import OpenSolid.Bounds2d qualified as Bounds2d
import OpenSolid.Bounds3d qualified as Bounds3d
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Prelude
import OpenSolid.Qty qualified as Qty
import OpenSolid.Units (Meters)
import OpenSolid.Vertex2d (Vertex2d)
import OpenSolid.Vertex2d qualified as Vertex2d
import OpenSolid.Vertex3d (Vertex3d)
import OpenSolid.Vertex3d qualified as Vertex3d

data Mesh vertex = Mesh (Array vertex) (List (Int, Int, Int))
  deriving (Eq, Show)

instance FFI (Mesh ()) where
  representation = FFI.classRepresentation "Mesh"

instance Vertex2d vertex (space @ units) => Bounded2d (Mesh vertex) (space @ units) where
  bounds mesh = Bounds2d.hullN (NonEmpty.map Vertex2d.position (Array.toNonEmpty (vertices mesh)))

instance Vertex3d vertex (space @ units) => Bounded3d (Mesh vertex) (space @ units) where
  bounds mesh = Bounds3d.hullN (NonEmpty.map Vertex3d.position (Array.toNonEmpty (vertices mesh)))

-- | A constraint on the quality of some mesh to be produced.
data Constraint units
  = MaxError (Qty units)
  | MaxSize (Qty units)

instance FFI (Constraint Meters) where
  representation = FFI.nestedClassRepresentation @(Mesh ()) Proxy "Constraint"

-- | Specify the maximum error/deviation of the mesh from the actual shape.
maxError :: Qty units -> Constraint units
maxError = MaxError

-- | Specify the maximum size of any triangle in the mesh.
maxSize :: Qty units -> Constraint units
maxSize = MaxSize

data Constraints units = Constraints
  { maxError :: Qty units
  , maxSize :: Qty units
  }

unconstrained :: Constraints units
unconstrained = Constraints{maxError = Qty.infinity, maxSize = Qty.infinity}

apply :: Constraints units -> Constraint units -> Constraints units
apply current (MaxError value) = current{maxError = value}
apply current (MaxSize value) = current{maxSize = value}

constraints :: NonEmpty (Constraint units) -> Constraints units
constraints nonEmpty = NonEmpty.foldl apply unconstrained nonEmpty

indexed :: Array vertex -> List (Int, Int, Int) -> Mesh vertex
indexed = Mesh

vertices :: Mesh vertex -> Array vertex
vertices (Mesh vs _) = vs

faceIndices :: Mesh vertex -> List (Int, Int, Int)
faceIndices (Mesh _ fs) = fs

faceVertices :: Mesh vertex -> List (vertex, vertex, vertex)
faceVertices (Mesh vs fs) = do
  let toVertices (i, j, k) = (Array.get i vs, Array.get j vs, Array.get k vs)
  List.map toVertices fs

map :: (a -> b) -> Mesh a -> Mesh b
map f (Mesh vs fs) = Mesh (Array.map f vs) fs

concat :: NonEmpty (Mesh a) -> Mesh a
concat meshes = do
  let vertexArrays = NonEmpty.map vertices meshes
  let faceIndexLists = NonEmpty.toList (NonEmpty.map faceIndices meshes)
  let arrayLengths = NonEmpty.toList (NonEmpty.map Array.length vertexArrays)
  let combinedVertices = Array.fromNonEmpty (NonEmpty.collect Array.toNonEmpty vertexArrays)
  let combinedFaceIndices = List.concat (offsetFaceIndices 0 arrayLengths faceIndexLists)
  Mesh combinedVertices combinedFaceIndices

collect :: Foldable1 list => (a -> Mesh b) -> list a -> Mesh b
collect toMesh values = concat (NonEmpty.map toMesh (Data.Foldable1.toNonEmpty values))

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
