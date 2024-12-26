module Python
  ( lines
  , indent
  , str
  , list
  , tuple
  , call
  , docstring
  )
where

import List qualified
import OpenSolid.Prelude
import OpenSolid.Text qualified as Text

lines :: List Text -> Text
lines = Text.multiline

indent :: List Text -> Text
indent = Text.multiline . List.map stripBlank . Text.lines . Text.indent "    " . lines

stripBlank :: Text -> Text
stripBlank text = if Text.strip text == "" then "" else text

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

docstring :: Text -> Text
docstring text = do
  let stripped = if Text.startsWith " " text then Text.drop 1 text else text
  let padded =
        if Text.contains "\n" stripped && not (Text.endsWith "\n" stripped)
          then stripped + "\n"
          else stripped
  "\"\"\"" + padded + "\"\"\""
