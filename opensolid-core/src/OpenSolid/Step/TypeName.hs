module OpenSolid.Step.TypeName (TypeName, fromText, toText) where

import OpenSolid.Prelude
import OpenSolid.Text qualified as Text

-- | A capitalized type name like "IFCWALL".
newtype TypeName = TypeName Text

-- | Construct a type name from a string. This wil capitalize the string.
fromText :: Text -> TypeName
fromText = TypeName . Text.toUpper

-- | Convert a type name to a string. The result will always be capitalized.
toText :: TypeName -> Text
toText (TypeName text) = text
