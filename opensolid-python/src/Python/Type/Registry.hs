module Python.Type.Registry
  ( Registry
  , empty
  , member
  , add
  , typeDeclarations
  )
where

import Data.Set (Set)
import Data.Set qualified as Set
import OpenSolid.List qualified as List
import OpenSolid.Pair qualified as Pair
import OpenSolid.Prelude
import Python qualified

data Registry = Registry (Set Text) (List (Text, Text))

empty :: Registry
empty = Registry Set.empty []

member :: Text -> Registry -> Bool
member typeName (Registry registered _) = Set.member typeName registered

add :: Text -> Text -> Registry -> Registry
add typeName typeDeclaration registry
  | member typeName registry = registry
  | otherwise = do
      let Registry registered tuples = registry
      Registry (Set.insert typeName registered) ((typeName, typeDeclaration) : tuples)

typeDeclarations :: Registry -> Text
typeDeclarations (Registry _ tuples) =
  Python.lines (List.reverseMap Pair.second tuples)
