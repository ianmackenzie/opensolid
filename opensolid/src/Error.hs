module Error
  ( Error (message)
  , Map (map)
  , toText
  , context
  , print
  , log
  , debug
  )
where

import Arithmetic
import Basics
import Composition
import Debug qualified
import {-# SOURCE #-} IO qualified
import {-# SOURCE #-} Text qualified
import Prelude qualified

class (Eq error, Show error) => Error error where
  message :: error -> Text
  message = Text.show

instance Error (List Char) where
  message = Text.pack

instance Error Text where
  message = identity

class (Error x, Error y) => Map x y m n | m -> x, n -> y, m y -> n where
  map :: (x -> y) -> m a -> n a

instance Map Text Text IO IO where
  map function = IO.onError (\error -> Prelude.fail (Text.unpack (function error)))

toText :: Map x Text m n => m a -> n a
toText = map message

context :: Map x Text m n => Text -> m a -> n a
context string = map (message >> addContext string)

addContext :: Text -> Text -> Text
addContext string text = string + ":\n" + Text.indent "  " text

print :: Map x x m m => Text -> m a -> m a
print output = map (\error -> do Debug.print output; error)

log :: (Map x x m m, Show b) => Text -> b -> m a -> m a
log label value = map (\error -> do Debug.log label value; error)

debug :: Map x x m m => (x -> IO ()) -> m a -> m a
debug callback = map (\error -> do Debug.io (callback error); error)
