module Text (pack, unpack, show, indent) where

import Basics

pack :: List Char -> Text
unpack :: Text -> List Char
show :: Show a => a -> Text
indent :: Text -> Text -> Text
