module String
  ( String
  , concat
  , join
  , fromInt
  , fromFloat
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
import Data.Char qualified
import Data.List qualified
import Data.String qualified
import Float (Float)
import List qualified
import Maybe qualified
import Qty (Qty (Qty))
import Result (Result)
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
  Maybe.orError ("Couldn't parse input as integer: " ++ input) $
    Text.Read.readMaybe input

toFloat :: String -> Result String Float
toFloat input =
  Maybe.orError ("Couldn't parse input as float: " ++ input) $
    Maybe.map Qty (Text.Read.readMaybe input)

lines :: String -> List String
lines = Data.String.lines

indent :: String -> String -> String
indent indentation = join "\n" . List.map (indentation ++) . lines

repeat :: Int -> String -> String
repeat n string = concat (List.repeat n string)

contains :: String -> String -> Bool
contains = Data.List.isInfixOf

toLower :: String -> String
toLower = List.map Data.Char.toLower

toUpper :: String -> String
toUpper = List.map Data.Char.toUpper
