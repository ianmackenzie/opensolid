-- Set in package.yaml, but hlint/HLS doesn't seem to notice it there
-- Can remove this if doing so no longer triggers warnings in ifThenElse
-- (e.g. an update to hlint or HLS notices the extension in package.yaml/opensolid.cabal)
{-# LANGUAGE Strict #-}

module Basics
  ( List
  , ifThenElse
  , fromString
  , fromInteger
  , otherwise
  , Int
  , Eq ((==), (/=))
  , Ord ((<), (<=), (>=), (>), min, max)
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
  ( Bool (..)
  , Char
  , Eq (..)
  , IO
  , IOError
  , Int
  , Maybe (..)
  , Ord (..)
  , Show
  , not
  , otherwise
  , ($)
  , (&&)
  , (||)
  )
import Prelude qualified

type List a = [a]

ifThenElse :: Bool -> a -> a -> a
ifThenElse True ifBranch ~_ = ifBranch
ifThenElse False ~_ elseBranch = elseBranch

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
