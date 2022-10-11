module Result (
    Result (..),
    map,
    map2,
    andThen,
    withDefault,
    mapError,
) where

import Prelude ((>>=))
import qualified Prelude

data Result x a
    = Ok !a
    | Err !x
    deriving (Prelude.Show, Prelude.Eq)

instance Prelude.Functor (Result x) where
    fmap function result =
        case result of
            Ok value -> Ok (function value)
            Err error -> Err error

instance Prelude.Applicative (Result x) where
    pure =
        Ok

    (<*>) result1 result2 =
        case (result1, result2) of
            (Ok function, Ok value) -> Ok (function value)
            (Err error, _) -> Err error
            (Ok _, Err error) -> Err error

instance Prelude.Monad (Result x) where
    (>>=) result function =
        case result of
            Ok value -> function value
            Err error -> Err error

withDefault :: a -> Result x a -> a
withDefault fallback result =
    case result of
        Ok value -> value
        Err _ -> fallback

map :: (a -> value) -> Result x a -> Result x value
map =
    Prelude.fmap

map2 :: (a -> b -> value) -> Result x a -> Result x b -> Result x value
map2 function result1 result2 = do
    value1 <- result1
    value2 <- result2
    Ok (function value1 value2)

andThen :: (a -> Result x b) -> Result x a -> Result x b
andThen function result =
    result >>= function

mapError :: (x -> y) -> Result x a -> Result y a
mapError function result =
    case result of
        Ok value -> Ok value
        Err error -> Err (function error)
