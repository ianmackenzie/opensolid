-- Needed for List and NonEmpty HasField instances
-- (should hopefully be safe having those instances here,
-- since pretty much *any* modules that use OpenSolid
-- will indirectly import this module, even if e.g.
-- they don't import OpenSolid.List or OpenSolid.NonEmpty)
{-# OPTIONS_GHC -Wno-orphans #-}

module OpenSolid.Bootstrap
  ( module Prelude
  , List
  , Text
  , ifThenElse
  , IsString (fromString)
  , fromLabel
  , Eq ((==), (/=))
  , Ord ((<), (<=), (>=), (>), compare)
  , Ordering (EQ, GT, LT)
  , FoldableWithIndex
  , TraversableWithIndex
  , Bool (True, False)
  , Async
  , Maybe (Just, Nothing)
  , Type
  , (@)
  , assert
  , internalError
  , exception
  , pattern TODO
  , (|>)
  , HasCallStack
  , withFrozenCallStack
  , HasField (getField)
  )
where

import Control.Concurrent.Async (Async)
import Control.Exception (assert)
import Data.Foldable.WithIndex (FoldableWithIndex)
import Data.Kind (Type)
import Data.List qualified
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text qualified
import Data.Traversable.WithIndex (TraversableWithIndex)
import GHC.OverloadedLabels (fromLabel)
import GHC.Records (HasField (getField))
import GHC.Stack (HasCallStack, withFrozenCallStack)
import Prelude (div, fromInteger, fromRational, mod)
import Prelude hiding
  ( Floating (..)
  , Fractional (..)
  , Integral (..)
  , Num (..)
  , Rational
  , Real (..)
  , RealFloat (..)
  , RealFrac (..)
  , drop
  , error
  , filter
  , head
  , init
  , last
  , length
  , lines
  , map
  , maximum
  , minimum
  , read
  , replicate
  , reverse
  , splitAt
  , subtract
  , sum
  , tail
  , take
  , unzip
  , unzip3
  , zip
  , zip3
  , zipWith
  , zipWith3
  , (.)
  , (^)
  , (^^)
  )
import Prelude qualified

type List a = [a]

{-# INLINE ifThenElse #-}
ifThenElse :: Bool -> a -> a -> a
ifThenElse True ifBranch _ = ifBranch
ifThenElse False _ elseBranch = elseBranch

internalError :: HasCallStack => Text -> a
internalError message = withFrozenCallStack $ abort ("Internal error: " <> message)

exception :: HasCallStack => Text -> a
exception message = withFrozenCallStack $ abort ("Exception occurred: " <> message)

pattern TODO :: HasCallStack => a
pattern TODO <- (withFrozenCallStack todo -> ())
  where
    TODO = withFrozenCallStack todo

{-# COMPLETE TODO #-}

todo :: HasCallStack => a
todo = withFrozenCallStack $ abort "Not implemented"

abort :: HasCallStack => Text -> a
abort message = Prelude.error (Data.Text.unpack message)

{-# INLINE (|>) #-}
(|>) :: a -> (a -> b) -> b
value |> function = function value

infixl 0 |>

{-# INLINE (@) #-}
(@) :: (a -> b) -> a -> b
function @ value = function value

infixl 1 @

instance HasField "length" (List a) Int where
  getField = Data.List.length

instance HasField "first" (NonEmpty a) a where
  getField = Data.List.NonEmpty.head

instance HasField "rest" (NonEmpty a) [a] where
  getField = Data.List.NonEmpty.tail

instance HasField "last" (NonEmpty a) a where
  getField = Data.List.NonEmpty.last

instance HasField "length" (NonEmpty a) Int where
  getField = Data.List.NonEmpty.length
