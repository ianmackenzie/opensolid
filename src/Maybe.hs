module Maybe
  ( map
  , withDefault
  , orError
  )
where

import OpenSolid
import Prelude qualified

map :: (a -> b) -> Maybe a -> Maybe b
map = Prelude.fmap

withDefault :: a -> Maybe a -> a
withDefault _ (Just value) = value
withDefault value Nothing = value

orError :: IsError x => x -> Maybe a -> Result x a
orError _ (Just value) = Ok value
orError error Nothing = Error error
