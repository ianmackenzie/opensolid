module OpenSolid.Step.Encode
  ( derived
  , null
  , bool
  , int
  , number
  , text
  , id
  , enum
  , binaryData
  , list
  , typeName
  , typedAttribute
  )
where

import Data.ByteString.Builder qualified
import OpenSolid.Binary (Builder)
import OpenSolid.Binary qualified as Binary
import OpenSolid.Prelude hiding (id)
import OpenSolid.Step.Binary qualified as Step.Binary
import OpenSolid.Step.EnumValue (EnumValue)
import OpenSolid.Step.EnumValue qualified as Step.EnumValue
import OpenSolid.Step.TypeName (TypeName)
import OpenSolid.Step.TypeName qualified as Step.TypeName
import OpenSolid.Text qualified as Text

char :: Char -> Builder
char = Data.ByteString.Builder.charUtf8

{-| Encode some text by wrapping it in single quotation marks,
and escaping both single quotation marks and backslashes.
-}
text :: Text -> Builder
text value = do
  let escapedValue = value & Text.replace "'" "''" & Text.replace "\\" "\\\\"
  char '\'' <> Text.toUtf8 escapedValue <> char '\''

{-| Encode binary data as a hex-encoded string according to the STEP standard,
and wrap it in double quotation marks.
-}
binaryData :: Builder -> Builder
binaryData builder = char '"' <> Step.Binary.builder builder <> char '"'

-- | Encode an enum value as capitalized text with leading and trailing periods.
enum :: EnumValue -> Builder
enum enumValue = char '.' <> Text.toUtf8 (Step.EnumValue.toText enumValue) <> char '.'

-- | Encode a boolean value as either '.T.' or '.F.'.
bool :: Bool -> Builder
bool True = Text.toUtf8 ".T."
bool False = Text.toUtf8 ".F."

-- | Encode an integer value.
int :: Int -> Builder
int = Text.toUtf8 . Text.int

{-| Encode a numeric value.

This will ensure that the encoded value has a trailing decimal point,
as required by the STEP standard.
-}
number :: Number -> Builder
number value = do
  let numberText = Text.number value
  let encodedNumber = Text.toUtf8 numberText
  if Text.contains "." numberText then encodedNumber else encodedNumber <> char '.'

-- | The special 'derived value' character '*'.
derived :: Builder
derived = char '*'

-- | The special 'null value' character '$'.
null :: Builder
null = char '$'

{-| Given a list of encoded attribute values,
join them using commas and enclose them in parentheses.
-}
list :: List Builder -> Builder
list attributeValues = char '(' <> Binary.join (char ',') attributeValues <> char ')'

-- | Encode a type name as text.
typeName :: TypeName -> Builder
typeName = Text.toUtf8 . Step.TypeName.toText

{-| Encode a typed attribute by surrounding an existing encoded attribute in parentheses
and prepending the given type name.
-}
typedAttribute :: TypeName -> Builder -> Builder
typedAttribute attributeType encodedAttribute =
  typeName attributeType <> char '(' <> encodedAttribute <> char ')'

-- | Encode a STEP integer ID as (for example) '#123'.
id :: Int -> Builder
id value = char '#' <> int value
