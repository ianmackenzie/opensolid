module OpenSolid.Step.EnumValue (EnumValue, fromText, toText) where

import OpenSolid.Prelude
import OpenSolid.Text qualified as Text

-- | A capitalized enum value such as "STEEL" or "METRE".
newtype EnumValue = EnumValue Text

{-| Construct an enum value from text.

The text will be capitalized and will have periods removed.
-}
fromText :: Text -> EnumValue
fromText text = EnumValue (text & Text.toUpper & Text.replace "." "")

{-| Convert an enum value to text.

The result will always be capitalized and will not have leading/trailing periods
(enum values are encoded in STEP files using leading and trailing periods,
but those periods are considered to be an encoding detail and not part of the enum value).
-}
toText :: EnumValue -> Text
toText (EnumValue text) = text
