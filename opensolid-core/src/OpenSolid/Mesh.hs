module OpenSolid.Mesh
  ( Mesh
  , indexed
  , vertices
  , faceIndices
  , faceVertices
  , map
  )
where

import OpenSolid.Array (Array)
import OpenSolid.Array qualified as Array
import OpenSolid.List qualified as List
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
