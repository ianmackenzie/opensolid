module OpenSolid.FFI.Exception
  ( Interface (..)
  , Function (..)
  , Exception (..)
  )
where

import Data.Proxy (Proxy)
import OpenSolid
import {-# SOURCE #-} OpenSolid.FFI (FFI)

class Interface x where
  code :: Proxy x -> Int
  name :: Proxy x -> Text
  message :: x -> Text
  functions :: Proxy x -> List (Function x)

data Function x where
  Function0 ::
    FFI result =>
    Text ->
    (x -> result) ->
    Function x
  Function1 ::
    (FFI a, FFI result) =>
    Text ->
    Text ->
    (a -> x -> result) ->
    Function x
  Function2 ::
    (FFI a, FFI b, FFI result) =>
    Text ->
    Text ->
    Text ->
    (a -> b -> x -> result) ->
    Function x
  Function3 ::
    (FFI a, FFI b, FFI c, FFI result) =>
    Text ->
    Text ->
    Text ->
    Text ->
    (a -> b -> c -> x -> result) ->
    Function x
  Function4 ::
    (FFI a, FFI b, FFI c, FFI d, FFI result) =>
    Text ->
    Text ->
    Text ->
    Text ->
    Text ->
    (a -> b -> c -> d -> x -> result) ->
    Function x

data Exception where
  Exception :: Interface x => x -> Exception
