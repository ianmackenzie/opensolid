module OpenSolid.Text (pack, unpack, show, multiline, indent) where

import Data.Text (Text)
import OpenSolid.List (List)
import Prelude (Char, Show)

pack :: List Char -> Text
unpack :: Text -> List Char
show :: Show a => a -> Text
multiline :: List Text -> Text
indent :: Text -> Text -> Text
