module String
  ( String
  , concat
  , join
  , fromInt
  , fromFloat
  , toInt
  , toFloat
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
import Float (Float)
import List qualified
import Qty (Qty (Qty))
import {-# SOURCE #-} Result (Result (Error, Ok))
import Text.Read qualified

concat :: List String -> String
concat = Data.List.concat

join :: String -> List String -> String
join = Data.List.intercalate

fromInt :: Int -> String
fromInt = show

fromFloat :: Float -> String
fromFloat (Qty x) = show x

toInt :: String -> Result String Int
toInt input =
  case Text.Read.readMaybe input of
    Just value -> Ok value
    Nothing -> Error ("Couldn't parse input as integer: " ++ input)

toFloat :: String -> Result String Float
toFloat input =
  case Text.Read.readMaybe input of
    Just value -> Ok value
    Nothing -> Error ("Couldn't parse input as float: " ++ input)

lines :: String -> List String
lines string =
  let (first, rest) = List.foldRight fold ("", []) string
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
