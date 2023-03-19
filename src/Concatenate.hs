module Concatenate (Concatenate ((++))) where

import List (List)
import Maybe (Maybe (Just, Nothing))
import Text (Text)
import Prelude qualified

class Concatenate a b c | a b -> c where
  (++) :: a -> b -> c

instance a ~ a' => Concatenate (List a) (List a') (List a) where
  (++) = Prelude.mappend

instance a ~ a' => Concatenate (Maybe a) (List a') (List a) where
  Just value ++ list = value : list
  Nothing ++ list = list

instance Concatenate Text Text Text where
  (++) = Prelude.mappend

instance Concatenate Text (Maybe Text) Text where
  text ++ Just suffix = text ++ suffix
  text ++ Nothing = text

instance Concatenate (Maybe Text) Text Text where
  Just prefix ++ text = prefix ++ text
  Nothing ++ text = text
