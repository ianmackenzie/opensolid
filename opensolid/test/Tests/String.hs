module Tests.String (tests) where

import OpenSolid
import String qualified
import Test (Test)
import Test qualified

tests :: List Test
tests =
  [ lines
  , multiline
  ]

lines :: Test
lines =
  Test.group "lines" <|
    [ test "Empty" "" [""]
    , test "Single line" "line" ["line"]
    , test "Two lines" "line1\nline2" ["line1", "line2"]
    , test "Trailing newline" "line1\nline2\n" ["line1", "line2", ""]
    ]
 where
  test label string expected =
    Test.verify label (Test.expect (String.lines string == expected))

multiline :: Test
multiline =
  Test.group "multiline" <|
    [ test "Empty list" [] ""
    , test "Single empty line" [""] ""
    , test "Single line" ["line"] "line"
    , test "Two lines" ["line1", "line2"] "line1\nline2"
    , test "Trailing newline" ["line1", "line2", ""] "line1\nline2\n"
    ]
 where
  test label list expected =
    Test.verify label (Test.expect (String.multiline list == expected))