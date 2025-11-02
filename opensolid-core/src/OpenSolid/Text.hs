module OpenSolid.Text
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
  , replicate
  , contains
  , startsWith
  , endsWith
  , drop
  , toLower
  , toUpper
  , capitalize
  , strip
  , toUtf8
  , InvalidUtf8 (InvalidUtf8)
  , decodeUtf8
  , assumeUtf8
  )
where

import Data.Char qualified
import Data.Text qualified
import Data.Text.Encoding qualified
import OpenSolid.Binary (Builder, ByteString)
import OpenSolid.Bootstrap hiding (concat)
import OpenSolid.Composition
import OpenSolid.Error qualified as Error
import OpenSolid.Float (Float)
import OpenSolid.Float qualified as Float
import OpenSolid.List qualified as List
import OpenSolid.Result (Result (Failure, Success))
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

split :: Text -> Text -> List Text
split = Data.Text.splitOn

lines :: Text -> List Text
lines = replace "\r\n" "\n" >> split "\n"

multiline :: List Text -> Text
multiline = join "\n"

indent :: Text -> Text -> Text
indent indentation paragraph =
  paragraph
    |> lines
    |> List.map (indentation <>)
    |> multiline

replicate :: Int -> Text -> Text
replicate = Data.Text.replicate

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

strip :: Text -> Text
strip = Data.Text.strip

toUtf8 :: Text -> Builder
toUtf8 = Data.Text.Encoding.encodeUtf8Builder

data InvalidUtf8 = InvalidUtf8 deriving (Eq, Show, Error.Message)

decodeUtf8 :: ByteString -> Result InvalidUtf8 Text
decodeUtf8 byteString =
  case Data.Text.Encoding.decodeUtf8' byteString of
    Prelude.Right text -> Success text
    Prelude.Left _ -> Failure InvalidUtf8

assumeUtf8 :: ByteString -> Text
assumeUtf8 = Data.Text.Encoding.decodeUtf8Lenient
