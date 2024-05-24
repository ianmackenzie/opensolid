module Text
  ( Text
  , pack
  , unpack
  , concat
  , join
  , int
  , float
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
  , toLower
  , toUpper
  )
where

import Arithmetic
import Basics
import Composition
import Data.Text qualified
import Float (Float)
import List qualified
import Qty (Qty (Qty_))
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
float (Qty_ x) = pack (Prelude.show x)

isEmpty :: Text -> Bool
isEmpty = Data.Text.null

replace :: Text -> Text -> Text -> Text
replace = Data.Text.replace

split :: Text -> Text -> List Text
split = Data.Text.splitOn

lines :: Text -> List Text
lines = replace (pack "\r\n") (pack "\n") >> split (pack "\n")

multiline :: List Text -> Text
multiline = join (pack "\n")

indent :: Text -> Text -> Text
indent indentation paragraph =
  paragraph
    |> lines
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

toLower :: Text -> Text
toLower = Data.Text.toLower

toUpper :: Text -> Text
toUpper = Data.Text.toUpper
