module Text (pack, unpack, show, multiline, indent) where

import Basics

pack :: List Char -> Text
unpack :: Text -> List Char
show :: Show a => a -> Text
multiline :: List Text -> Text
indent :: Text -> Text -> Text
