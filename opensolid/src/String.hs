module String
  ( String
  , concat
  , join
  , fromInt
  , fromFloat
  , lines
  , multiline
  , indent
  , repeat
  , contains
  , toLower
  , toUpper
  )
where

import Basics
import Concatenation
import Data.Char qualified
import Data.List qualified
import {-# SOURCE #-} Float (Float)
import List qualified
import Qty (Qty (Qty_))

concat :: List String -> String
concat = Data.List.concat

join :: String -> List String -> String
join = Data.List.intercalate

fromInt :: Int -> String
fromInt = show

fromFloat :: Float -> String
fromFloat (Qty_ x) = show x

lines :: String -> List String
lines string =
  let (first, rest) = List.foldr fold ("", []) string
   in first : rest
 where
  fold character (current, accumulated)
    | character == '\n' = ("", current : accumulated)
    | otherwise = (character : current, accumulated)

multiline :: List String -> String
multiline = join "\n"

indent :: String -> String -> String
indent indentation paragraph =
  paragraph
    |> lines
    |> List.map (indentation ++)
    |> multiline

repeat :: Int -> String -> String
repeat n string = concat (List.repeat n string)

contains :: String -> String -> Bool
contains = Data.List.isInfixOf

toLower :: String -> String
toLower = List.map Data.Char.toLower

toUpper :: String -> String
toUpper = List.map Data.Char.toUpper
