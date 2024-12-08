module Text
  ( Text
  , pack
  , unpack
  , concat
  , join
  , int
  , float
  , char
  , show
  , isEmpty
  , replace
  , split
  , lines
  , multiline
  , indent
  , repeat
  , contains
  , startsWith
  , endsWith
  , drop
  , toLower
  , toUpper
  , capitalize
  , encodeUtf8
  , InvalidUtf8 (InvalidUtf8)
  , decodeUtf8
  , assumeUtf8
  )
where

import Arithmetic
import Basics
import Composition
import Data.ByteString (ByteString)
import Data.Char qualified
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified
import Data.Text qualified
import Data.Text.Encoding qualified
import Error qualified
import Float (Float)
import Float qualified
import List qualified
import Result (Result (Failure, Success))
import Prelude qualified

concat :: List Text -> Text
concat = Data.Text.concat

join :: Text -> List Text -> Text
join = Data.Text.intercalate

pack :: List Char -> Text
pack = Data.Text.pack

unpack :: Text -> List Char
unpack = Data.Text.unpack

int :: Int -> Text
int n = pack (Prelude.show n)

float :: Float -> Text
float x = pack (Prelude.show (Float.toDouble x))

char :: Char -> Text
char c = pack [c]

show :: Show a => a -> Text
show = Prelude.show >> pack

isEmpty :: Text -> Bool
isEmpty = Data.Text.null

replace :: Text -> Text -> Text -> Text
replace = Data.Text.replace

split :: Text -> Text -> NonEmpty Text
split separator text = case Data.Text.splitOn separator text of
  first : rest -> first :| rest
  [] -> internalError "Splitting text should never give an empty list"

lines :: Text -> NonEmpty Text
lines = replace "\r\n" "\n" >> split "\n"

multiline :: List Text -> Text
multiline = join "\n"

indent :: Text -> Text -> Text
indent indentation paragraph =
  paragraph
    |> lines
    |> Data.List.NonEmpty.toList
    |> List.map (indentation +)
    |> multiline

repeat :: Int -> Text -> Text
repeat = Data.Text.replicate

contains :: Text -> Text -> Bool
contains = Data.Text.isInfixOf

startsWith :: Text -> Text -> Bool
startsWith = Data.Text.isPrefixOf

endsWith :: Text -> Text -> Bool
endsWith = Data.Text.isSuffixOf

drop :: Int -> Text -> Text
drop = Data.Text.drop

toLower :: Text -> Text
toLower = Data.Text.toLower

toUpper :: Text -> Text
toUpper = Data.Text.toUpper

capitalize :: Text -> Text
capitalize text =
  case Data.Text.uncons text of
    Just (first, rest) -> Data.Text.cons (Data.Char.toUpper first) rest
    Nothing -> text

encodeUtf8 :: Text -> ByteString
encodeUtf8 = Data.Text.Encoding.encodeUtf8

data InvalidUtf8 = InvalidUtf8 deriving (Eq, Show, Error.Message)

decodeUtf8 :: ByteString -> Result InvalidUtf8 Text
decodeUtf8 byteString =
  case Data.Text.Encoding.decodeUtf8' byteString of
    Prelude.Right text -> Success text
    Prelude.Left _ -> Failure InvalidUtf8

assumeUtf8 :: ByteString -> Text
assumeUtf8 = Data.Text.Encoding.decodeUtf8Lenient
