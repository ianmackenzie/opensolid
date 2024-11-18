module TypeRegistry
  ( TypeRegistry
  , empty
  , contains
  , add
  , typeDeclarations
  )
where

import Data.Set (Set)
import Data.Set qualified as Set
import List qualified
import OpenSolid
import Pair qualified
import Python qualified

data TypeRegistry = TypeRegistry (Set Text) (List (Text, Text))

empty :: TypeRegistry
empty = TypeRegistry Set.empty []

contains :: Text -> TypeRegistry -> Bool
contains typeName (TypeRegistry registered _) = Set.member typeName registered

add :: Text -> Text -> TypeRegistry -> TypeRegistry
add typeName typeDeclaration registry
  | contains typeName registry = registry
  | otherwise = do
      let TypeRegistry registered tuples = registry
      TypeRegistry (Set.insert typeName registered) ((typeName, typeDeclaration) : tuples)

typeDeclarations :: TypeRegistry -> Text
typeDeclarations (TypeRegistry _ tuples) =
  Python.separate (List.reverseMap Pair.second tuples)
