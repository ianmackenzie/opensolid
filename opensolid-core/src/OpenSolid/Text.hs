module OpenSolid.Text
  ( Text
  , pack
  , unpack
  , concat
  , join
  , int
  , number
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
import Data.Text (Text)
import Data.Text qualified
import Data.Text.Encoding qualified
import OpenSolid.Binary (Builder, ByteString)
import OpenSolid.Error qualified as Error
import OpenSolid.List (List)
import OpenSolid.List qualified as List
import OpenSolid.Number (Number)
import OpenSolid.Result (Result (Failure, Success))
import Prelude
  ( Bool
  , Char
  , Either (Left, Right)
  , Eq
  , Int
  , Maybe (Just, Nothing)
  , Show
  , (<>)
  )
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
int = show

number :: Number -> Text
number = show

char :: Char -> Text
char c = pack [c]

show :: Show a => a -> Text
show value = pack (Prelude.show value)

isEmpty :: Text -> Bool
isEmpty = Data.Text.null

replace :: Text -> Text -> Text -> Text
replace = Data.Text.replace

split :: Text -> Text -> List Text
split = Data.Text.splitOn

lines :: Text -> List Text
lines text = split "\n" (replace "\r\n" "\n" text)

multiline :: List Text -> Text
multiline = join "\n"

indent :: Text -> Text -> Text
indent indentation paragraph =
  multiline (List.map (indentation <>) (lines paragraph))

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
    Right text -> Success text
    Left _ -> Failure InvalidUtf8

assumeUtf8 :: ByteString -> Text
assumeUtf8 = Data.Text.Encoding.decodeUtf8Lenient
