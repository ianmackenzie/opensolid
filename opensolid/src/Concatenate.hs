module Concatenate (Concatenate ((++))) where

import Basics
import Data.List.NonEmpty (NonEmpty ((:|)))
import Prelude qualified

class Concatenate a b c | a b -> c where
  (++) :: a -> b -> c

instance (a ~ a') => Concatenate (List a) (List a') (List a) where
  (++) = Prelude.mappend

instance (a ~ a') => Concatenate (NonEmpty a) (NonEmpty a') (NonEmpty a) where
  (a :| as) ++ (b :| bs) = a :| (as ++ (b : bs))

instance (a ~ a') => Concatenate (NonEmpty a) (List a') (NonEmpty a) where
  nonEmpty ++ [] = nonEmpty
  (a :| as) ++ bs = a :| (as ++ bs)

instance (a ~ a') => Concatenate (List a) (NonEmpty a') (NonEmpty a) where
  [] ++ nonEmpty = nonEmpty
  (a : as) ++ (b :| bs) = a :| (as ++ (b : bs))

instance (a ~ a') => Concatenate (Maybe a) (List a') (List a) where
  Just value ++ list = value : list
  Nothing ++ list = list

instance (a ~ a') => Concatenate (List a) (Maybe a') (List a) where
  list ++ Nothing = list
  list ++ Just value = list ++ [value]

instance (a ~ a') => Concatenate (Maybe a) (NonEmpty a') (NonEmpty a) where
  Just a ++ (b :| bs) = a :| (b : bs)
  Nothing ++ nonEmpty = nonEmpty

instance (a ~ a') => Concatenate (NonEmpty a) (Maybe a') (NonEmpty a) where
  nonEmpty ++ Nothing = nonEmpty
  nonEmpty ++ Just value = nonEmpty ++ [value]
