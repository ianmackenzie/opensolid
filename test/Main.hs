module Main (main) where

import Length qualified
import OpenSolid
import Task qualified
import Test (Test)
import Test qualified
import Tests.VectorBox3d qualified

tests :: List Test
tests =
  [ Tests.VectorBox3d.tests
  ]
 where
  ?tolerance = Length.meters 1e-9

main :: IO ()
main = Task.toIO (Test.run tests)
