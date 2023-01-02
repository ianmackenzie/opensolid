module Text (
    concat,
    join,
    fromChars,
    toChars,
    fromInt,
    fromFloat,
) where

import Data.Text qualified
import OpenSolid
import TextShow qualified

concat :: List Text -> Text
concat = Data.Text.concat

join :: Text -> List Text -> Text
join = Data.Text.intercalate

fromChars :: List Char -> Text
fromChars = Data.Text.pack

toChars :: Text -> List Char
toChars = Data.Text.unpack

fromInt :: Int -> Text
fromInt (Nbr n) = TextShow.showt n

fromFloat :: Float -> Text
fromFloat (Qty x) = TextShow.showt x
