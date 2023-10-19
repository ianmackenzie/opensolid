module Text
  ( Text
  , concat
  , join
  , fromChars
  , toChars
  , toString
  , fromInt
  , fromFloat
  , replace
  , toInt
  , toFloat
  , lines
  , indent
  , repeat
  , contains
  , toLower
  , toUpper
  )
where

import Basics
import Concatenate
import Data.String qualified
import Data.Text qualified
import Data.Text.Read
import Float (Float)
import List qualified
import Qty (Qty (Qty))
import Result (Result (Error, Ok))
import Result qualified
import Prelude qualified

concat :: List Text -> Text
concat = Data.Text.concat

join :: Text -> List Text -> Text
join = Data.Text.intercalate

fromChars :: List Char -> Text
fromChars = Data.Text.pack

toChars :: Text -> List Char
toChars = Data.Text.unpack

toString :: (Data.String.IsString a) => Text -> a
toString text = Data.String.fromString (toChars text)

fromInt :: Int -> Text
fromInt n = Text.fromChars (Prelude.show n)

fromFloat :: Float -> Text
fromFloat (Qty x) = Text.fromChars (Prelude.show x)

toNum :: (Prelude.Num a) => Reader a -> Text -> Result Text a
toNum reader text =
  case Data.Text.Read.signed reader text of
    Prelude.Right (value, "") -> Ok value
    Prelude.Right (_, suffix) ->
      Error ("Could not parse '" ++ text ++ "' as a number - has extra trailing text '" ++ suffix)
    Prelude.Left message ->
      Error (fromChars message)

toInt :: Text -> Result Text Int
toInt = toNum Data.Text.Read.decimal

toFloat :: Text -> Result Text Float
toFloat text = Result.map Qty (toNum Data.Text.Read.double text)

replace :: Text -> Text -> Text -> Text
replace = Data.Text.replace

lines :: Text -> List Text
lines = Data.Text.lines

indent :: Text -> Text -> Text
indent indentation text =
  concat (indentation : (lines text |> List.intersperse ("\n" ++ indentation)))

repeat :: Int -> Text -> Text
repeat = Data.Text.replicate

contains :: Text -> Text -> Bool
contains = Data.Text.isInfixOf

toLower :: Text -> Text
toLower = Data.Text.toLower

toUpper :: Text -> Text
toUpper = Data.Text.toUpper
