module OpenSolid.FFI.Exception
  ( Interface (..)
  , MemberFunction (..)
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
  memberFunctions :: Proxy x -> List (MemberFunction x)

data MemberFunction x where
  MemberFunction0 ::
    FFI result =>
    Text ->
    (x -> result) ->
    MemberFunction x
  MemberFunction1 ::
    (FFI a, FFI result) =>
    Text ->
    Text ->
    (a -> x -> result) ->
    MemberFunction x
  MemberFunction2 ::
    (FFI a, FFI b, FFI result) =>
    Text ->
    Text ->
    Text ->
    (a -> b -> x -> result) ->
    MemberFunction x
  MemberFunction3 ::
    (FFI a, FFI b, FFI c, FFI result) =>
    Text ->
    Text ->
    Text ->
    Text ->
    (a -> b -> c -> x -> result) ->
    MemberFunction x
  MemberFunction4 ::
    (FFI a, FFI b, FFI c, FFI d, FFI result) =>
    Text ->
    Text ->
    Text ->
    Text ->
    Text ->
    (a -> b -> c -> d -> x -> result) ->
    MemberFunction x

data Exception where
  Exception :: Interface x => x -> Exception
