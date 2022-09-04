module String (
    concat,
    join,
    fromList,
    toList,
    fromInt,
    fromFloat,
) where

import qualified Data.Text
import OpenSolid
import qualified Prelude

concat :: List String -> String
concat =
    Data.Text.concat

join :: String -> List String -> String
join =
    Data.Text.intercalate

fromList :: List Char -> String
fromList =
    Data.Text.pack

toList :: String -> List Char
toList =
    Data.Text.unpack

fromInt :: Int -> String
fromInt (Count n) =
    fromList (show n)

fromFloat :: Float -> String
fromFloat (Quantity x) =
    fromList (show x)
