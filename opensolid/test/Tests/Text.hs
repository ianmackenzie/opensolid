module Tests.Text (tests) where

import OpenSolid
import Test (Test)
import Test qualified
import Text qualified

tests :: List Test
tests =
  [ lines
  , multiline
  ]

lines :: Test
lines = do
  let test label text expected =
        Test.verify label (Test.expect (Text.lines text == expected))
  Test.group "lines" $
    [ test "Empty" "" [""]
    , test "Single line" "line" ["line"]
    , test "Two lines" "line1\nline2" ["line1", "line2"]
    , test "Trailing newline" "line1\nline2\n" ["line1", "line2", ""]
    ]

multiline :: Test
multiline = do
  let test label list expected =
        Test.verify label (Test.expect (Text.multiline list == expected))
  Test.group "multiline" $
    [ test "Empty list" [] ""
    , test "Single empty line" [""] ""
    , test "Single line" ["line"] "line"
    , test "Two lines" ["line1", "line2"] "line1\nline2"
    , test "Trailing newline" ["line1", "line2", ""] "line1\nline2\n"
    ]
