module Basics
  ( List
  , ifThenElse
  , fromString
  , fromInteger
  , otherwise
  , Int
  , Eq ((==), (/=))
  , Ord ((<), (<=), (>=), (>))
  , Show
  , Bool (True, False)
  , Char
  , not
  , ($)
  , (&&)
  , (||)
  , IO
  , IOError
  , Maybe (Just, Nothing)
  , Type
  , HasField (getField)
  , Text
  , identity
  , always
  , flip
  , internalError
  , notImplemented
  , (|>)
  )
where

import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified
import GHC.Records (HasField (getField))
import Prelude
  ( Bool (False, True)
  , Char
  , Eq ((/=), (==))
  , IO
  , IOError
  , Int
  , Maybe (Just, Nothing)
  , Ord ((<), (<=), (>), (>=))
  , Show
  , flip
  , not
  , otherwise
  , ($)
  , (&&)
  , (||)
  )
import Prelude qualified

type List a = [a]

ifThenElse :: Bool -> a -> a -> a
ifThenElse True ifBranch _ = ifBranch
ifThenElse False _ elseBranch = elseBranch

fromString :: Prelude.String -> Text
fromString = Data.Text.pack

{-# INLINE fromInteger #-}
fromInteger :: Prelude.Integer -> Int
fromInteger = Prelude.fromInteger

identity :: a -> a
identity = Prelude.id

always :: a -> b -> a
always = Prelude.const

internalError :: Text -> a
internalError message = Prelude.error (Data.Text.unpack message)

notImplemented :: a
notImplemented = internalError "Not implemented"

{-# INLINE (|>) #-}
(|>) :: a -> (a -> b) -> b
(|>) value function = function value

infixl 0 |>
