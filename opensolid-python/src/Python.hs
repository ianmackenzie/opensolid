module Python
  ( lines
  , indent
  , separate
  , str
  , list
  , tuple
  , call
  , pattern Name
  )
where

import OpenSolid
import Text qualified

{-# COMPLETE Name #-}

pattern Name :: Text -> Text
pattern Name converted <- (name -> converted)

lines :: List Text -> Text
lines = Text.multiline

indent :: List Text -> Text
indent = Text.indent "    " . lines

separate :: List Text -> Text
separate blocks = Text.join "\n\n" blocks

str :: Text -> Text
str contents = "\"" + contents + "\""

list :: List Text -> Text
list entries = "[" + Text.join "," entries + "]"

tuple :: List Text -> Text
tuple [] = "()"
tuple [entry] = "(" + entry + ",)"
tuple entries = "(" + Text.join "," entries + ")"

call :: Text -> List Text -> Text
call function arguments = function + "(" + Text.join "," arguments + ")"

name :: Text -> Text
name = Text.replace " " "_"
