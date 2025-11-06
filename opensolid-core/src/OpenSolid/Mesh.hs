module OpenSolid.Mesh
  ( Mesh
  , empty
  , indexed
  , faceVertices
  , map
  , concat
  , combine
  , grid
  , indexedGrid
  )
where

import Data.Foldable1 (Foldable1)
import Data.Foldable1 qualified
import OpenSolid.Array (Array)
import OpenSolid.Array qualified as Array
import OpenSolid.Int qualified as Int
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Point2d (Point2d (Point2d))
import OpenSolid.Prelude hiding (concat, (*), (+), (-))
import OpenSolid.UvPoint (UvPoint)
import Prelude ((*), (+), (-))

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

empty :: Mesh vertex
empty = Mesh Array.empty []

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
  let combinedVertices = Array.fromList (List.combine Array.toList vertexArrays)
  let combinedFaceIndices = List.concat (offsetFaceIndices 0 arrayLengths faceIndexLists)
  Mesh combinedVertices combinedFaceIndices

combine :: Foldable1 list => (a -> Mesh b) -> list a -> Mesh b
combine toMesh values = concat (NonEmpty.map toMesh (Data.Foldable1.toNonEmpty values))

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

grid :: Int -> Int -> (UvPoint -> vertex) -> Mesh vertex
grid uSteps vSteps function =
  indexedGrid uSteps vSteps (toIndexedFunction uSteps vSteps function)

toIndexedFunction :: Int -> Int -> (UvPoint -> vertex) -> Int -> Int -> vertex
toIndexedFunction uSteps vSteps function uIndex vIndex =
  function (Point2d (Int.ratio uIndex uSteps) (Int.ratio vIndex vSteps))

indexedGrid :: Int -> Int -> (Int -> Int -> vertex) -> Mesh vertex
indexedGrid uSteps vSteps function =
  gridImpl uSteps vSteps (uSteps + 1) (vSteps + 1) function

gridImpl :: Int -> Int -> Int -> Int -> (Int -> Int -> vertex) -> Mesh vertex
gridImpl uSteps vSteps uVertices vVertices function =
  if uVertices <= 1 || vVertices <= 1
    then empty
    else do
      let numVertices = uVertices * vVertices
      let vertices =
            Array.initialize numVertices $
              \i -> function (i `mod` uVertices) (i `div` uVertices)
      let faceIndices = gridFaceIndices uSteps uVertices vVertices (uSteps - 1) (vSteps - 1) []
      Mesh vertices faceIndices

gridFaceIndices :: Int -> Int -> Int -> Int -> Int -> List (Int, Int, Int) -> List (Int, Int, Int)
gridFaceIndices uSteps uVertices vVertices uIndex0 vIndex0 accumulatedIndices = do
  let rowStart0 = uVertices * vIndex0
  let rowStart1 = uVertices * ((vIndex0 + 1) `mod` vVertices)
  let uIndex1 = (uIndex0 + 1) `mod` uVertices
  let index00 = rowStart0 + uIndex0
  let index10 = rowStart0 + uIndex1
  let index01 = rowStart1 + uIndex0
  let index11 = rowStart1 + uIndex1
  let lowerFaceIndices = (index00, index10, index11)
  let upperFaceIndices = (index00, index11, index01)
  let updatedIndices = lowerFaceIndices : upperFaceIndices : accumulatedIndices
  if
    | uIndex0 > 0 ->
        gridFaceIndices uSteps uVertices vVertices (uIndex0 - 1) vIndex0 updatedIndices
    | vIndex0 > 0 ->
        gridFaceIndices uSteps uVertices vVertices (uSteps - 1) (vIndex0 - 1) updatedIndices
    | otherwise -> updatedIndices
