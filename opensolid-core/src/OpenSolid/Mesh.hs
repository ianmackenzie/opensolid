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

import OpenSolid.Array (Array)
import OpenSolid.Array qualified as Array
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Prelude
import OpenSolid.Qty qualified as Qty

data Mesh vertex = Mesh (Array vertex) (List (Int, Int, Int))
  deriving (Eq, Show)

data Constraint units
  = MaxError (Qty units)
  | MaxSize (Qty units)

maxError :: Qty units -> Constraint units
maxError = MaxError

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

collect :: (a -> Mesh b) -> NonEmpty a -> Mesh b
collect toMesh values = concat (NonEmpty.map toMesh values)

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
