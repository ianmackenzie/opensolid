module Codegen
  ( map
  , sequence
  , collect
  )
where

import Language.Haskell.TH qualified as TH
import List qualified
import OpenSolid
import Prelude qualified

map :: (a -> b) -> TH.Q a -> TH.Q b
map = Prelude.fmap

sequence :: List (TH.Q a) -> TH.Q (List a)
sequence [] = return []
sequence (first : rest) = do
  firstValue <- first
  restValues <- sequence rest
  return (firstValue : restValues)

collect :: (a -> TH.Q b) -> List a -> TH.Q (List b)
collect function values = sequence (List.map function values)
