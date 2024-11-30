module Python
  ( lines
  , indent
  , str
  , list
  , tuple
  , call
  )
where

import OpenSolid
import Text qualified

lines :: List Text -> Text
lines = Text.multiline

indent :: List Text -> Text
indent = Text.indent "    " . lines

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
