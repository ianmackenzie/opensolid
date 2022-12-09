module String (
    concat,
    join,
    fromList,
    toList,
    fromInt,
    fromFloat,
) where

import Data.Text qualified
import OpenSolid
import TextShow qualified

concat :: List String -> String
concat = Data.Text.concat

join :: String -> List String -> String
join = Data.Text.intercalate

fromList :: List Char -> String
fromList = Data.Text.pack

toList :: String -> List Char
toList = Data.Text.unpack

fromInt :: Int -> String
fromInt (Nbr n) = TextShow.showt n

fromFloat :: Float -> String
fromFloat (Qty x) = TextShow.showt x
