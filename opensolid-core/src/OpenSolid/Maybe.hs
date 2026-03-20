module OpenSolid.Maybe
  ( Maybe (Just, Nothing)
  , map
  , map2
  , oneOf
  , collect
  , isJust
  , isNothing
  , orFail
  )
where

import Data.Text qualified
import OpenSolid.Prelude
import Prelude qualified

map :: (a -> b) -> Maybe a -> Maybe b
map = Prelude.fmap

map2 :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
map2 function (Just first) (Just second) = Just (function first second)
map2 _ Nothing _ = Nothing
map2 _ _ Nothing = Nothing

oneOf :: List (Maybe a) -> Maybe a
oneOf maybes = case maybes of
  Just value : _ -> Just value
  Nothing : rest -> oneOf rest
  [] -> Nothing

collect :: Traversable list => (a -> Maybe b) -> list a -> Maybe (list b)
collect = Prelude.mapM

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing (Just _) = False

orFail :: MonadFail m => Text -> Maybe a -> m a
orFail _ (Just value) = Prelude.return value
orFail message Nothing = Prelude.fail (Data.Text.unpack message)
