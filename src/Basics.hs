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
  , (++)
  , Maybe (Just, Nothing)
  , Type
  , Constraint
  , Symbol
  , HasField (getField)
  , Text
  , identity
  , always
  , internalError
  , notImplemented
  , (|>)
  , Named (Named)
  , fromLabel
  )
where

import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Data.Text qualified
import GHC.Records (HasField (getField))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
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

(++) :: Prelude.Monoid a => a -> a -> a
(++) = Prelude.mappend

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

newtype Named (name :: Symbol) value = Named value deriving (Eq)

instance (KnownSymbol name, Show value) => Show (Named name value) where
  showsPrec precedence (Named value) =
    let string = '#' : symbolVal (Proxy :: Proxy name) ++ (' ' : Prelude.showsPrec 10 value [])
     in Prelude.showParen (precedence >= 10) (Prelude.showString string)

class Nameable (name :: Symbol) value where
  fromLabel :: value -> Named name value
  fromLabel = Named

instance Nameable (name :: Symbol) value
