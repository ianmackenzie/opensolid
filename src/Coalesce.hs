-- Set in package.yaml, but hlint/HLS doesn't seem to notice it there
-- Can remove this if doing so no longer triggers warnings in ~_ patterns below
-- (e.g. an update to hlint or HLS notices the extension in package.yaml/opensolid.cabal)
{-# LANGUAGE Strict #-}

module Coalesce (Coalesce ((??))) where

import Maybe (Maybe (Just, Nothing))
import Result (Result (Error, Ok))

class Coalesce a b c | a b -> c where
  (??) :: a -> b -> c

infixl 0 ??

instance a ~ a' => Coalesce (Maybe a) (Maybe a') (Maybe a) where
  Just value ?? ~_ = Just value
  Nothing ?? fallback = fallback

instance a ~ a' => Coalesce (Maybe a) (Result x a') (Result x a) where
  Just value ?? ~_ = Ok value
  Nothing ?? fallback = fallback

instance a ~ a' => Coalesce (Result x a) (Maybe a') (Maybe a) where
  Ok value ?? ~_ = Just value
  Error ~_ ?? fallback = fallback

instance a ~ a' => Coalesce (Result x a) (Result y a') (Result y a) where
  Ok value ?? ~_ = Ok value
  Error ~_ ?? fallback = fallback
