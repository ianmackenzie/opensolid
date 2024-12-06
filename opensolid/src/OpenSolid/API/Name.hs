module OpenSolid.API.Name
  ( Name
  , parse
  , camelCase
  , pascalCase
  , snakeCase
  )
where

import Data.Char qualified
import Data.Text qualified
import List qualified
import NonEmpty qualified
import OpenSolid
import Text qualified

newtype Name = Name (NonEmpty Text) deriving (Eq, Ord, Show)

parse :: Text -> Name
parse input = do
  let components = Text.split " " input
  if NonEmpty.allSatisfy isCapitalized components
    then Name components
    else internalError ("API name has non-capitalized component: " + input)

isCapitalized :: Text -> Bool
isCapitalized component = case Data.Text.uncons component of
  Just (first, _) -> Data.Char.isUpper first || Data.Char.isNumber first
  Nothing -> internalError "API name component should never be empty"

pascalCase :: Name -> Text
pascalCase (Name components) = Text.concat (NonEmpty.toList components)

camelCase :: Name -> Text
camelCase (Name (first :| rest)) = Text.toLower first + Text.concat rest

snakeCase :: Name -> Text
snakeCase (Name components) = Text.join "_" (List.map Text.toLower (NonEmpty.toList components))
