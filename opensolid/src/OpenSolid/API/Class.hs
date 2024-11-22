module OpenSolid.API.Class
  ( Class (..)
  , Constructor (..)
  , StaticFunction (..)
  , MemberFunction (..)
  , Constraint (..)
  , numConstructors
  , numStaticFunctions
  , numMemberFunctions
  , withConstructors
  , mapStaticFunctions
  , mapMemberFunctions
  , callConstructor
  , callStaticFunction
  , callMemberFunction
  )
where

import Data.Kind qualified
import Foreign (Ptr)
import IO qualified
import Int qualified
import List qualified
import OpenSolid
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import Pair qualified
import Tolerance qualified
import Units (Meters)

data Class where
  Class ::
    FFI value =>
    { name :: Text
    , constructors :: List (Constructor value)
    , staticFunctions :: List (Text, List StaticFunction)
    , memberFunctions :: List (Text, List (MemberFunction value))
    } ->
    Class

numConstructors :: Class -> Int
numConstructors Class{constructors} =
  List.length constructors

numStaticFunctions :: Class -> Int
numStaticFunctions Class{staticFunctions} =
  Int.sumOf (List.length . Pair.second) staticFunctions

numMemberFunctions :: Class -> Int
numMemberFunctions Class{memberFunctions} =
  Int.sumOf (List.length . Pair.second) memberFunctions

withConstructors :: Class -> (forall value. FFI value => List (Int, Constructor value) -> a) -> a
withConstructors Class{constructors} callback = callback (List.indexed constructors)

mapStaticFunctions :: Class -> (Text -> List (Int, StaticFunction) -> a) -> List a
mapStaticFunctions cls@Class{staticFunctions} callback =
  mapStaticFunctionsImpl (numConstructors cls) staticFunctions callback

mapStaticFunctionsImpl ::
  Int ->
  List (Text, List StaticFunction) ->
  (Text -> List (Int, StaticFunction) -> a) ->
  List a
mapStaticFunctionsImpl startIndex memberFunctions callback = case memberFunctions of
  [] -> []
  (name, overloads) : rest ->
    callback name (List.zip2 [startIndex ..] overloads)
      : mapStaticFunctionsImpl (startIndex + List.length overloads) rest callback

mapMemberFunctions ::
  Class ->
  (forall value. FFI value => Text -> List (Int, MemberFunction value) -> a) ->
  List a
mapMemberFunctions cls@Class{memberFunctions} callback =
  mapMemberFunctionsImpl (numConstructors cls + numStaticFunctions cls) memberFunctions callback

mapMemberFunctionsImpl ::
  FFI value =>
  Int ->
  List (Text, List (MemberFunction value)) ->
  (Text -> List (Int, MemberFunction value) -> a) ->
  List a
mapMemberFunctionsImpl startIndex memberFunctions callback = case memberFunctions of
  [] -> []
  (name, overloads) : rest ->
    callback name (List.zip2 [startIndex ..] overloads)
      : mapMemberFunctionsImpl (startIndex + List.length overloads) rest callback

data Constraint (constraint :: Data.Kind.Constraint) where
  N :: Constraint ()
  F :: Constraint (Tolerance Unitless)
  L :: Constraint (Tolerance Meters)

data Constructor value where
  C0 ::
    FFI value =>
    Constraint constraint ->
    (constraint => value) ->
    Constructor value
  C1 ::
    (FFI a, FFI value) =>
    Constraint constraint ->
    Text ->
    (constraint => a -> value) ->
    Constructor value
  C2 ::
    (FFI a, FFI b, FFI value) =>
    Constraint constraint ->
    Text ->
    Text ->
    (constraint => a -> b -> value) ->
    Constructor value
  C3 ::
    (FFI a, FFI b, FFI c, FFI value) =>
    Constraint constraint ->
    Text ->
    Text ->
    Text ->
    (constraint => a -> b -> c -> value) ->
    Constructor value
  C4 ::
    (FFI a, FFI b, FFI c, FFI d, FFI value) =>
    Constraint constraint ->
    Text ->
    Text ->
    Text ->
    Text ->
    (constraint => a -> b -> c -> d -> value) ->
    Constructor value

data StaticFunction where
  S0 ::
    FFI a =>
    Constraint constraint ->
    (constraint => a) ->
    StaticFunction
  S1 ::
    (FFI a, FFI b) =>
    Constraint constraint ->
    Text ->
    (constraint => a -> b) ->
    StaticFunction
  S2 ::
    (FFI a, FFI b, FFI c) =>
    Constraint constraint ->
    Text ->
    Text ->
    (constraint => a -> b -> c) ->
    StaticFunction
  S3 ::
    (FFI a, FFI b, FFI c, FFI d) =>
    Constraint constraint ->
    Text ->
    Text ->
    Text ->
    (constraint => a -> b -> c -> d) ->
    StaticFunction
  S4 ::
    (FFI a, FFI b, FFI c, FFI d, FFI e) =>
    Constraint constraint ->
    Text ->
    Text ->
    Text ->
    Text ->
    (constraint => a -> b -> c -> d -> e) ->
    StaticFunction

data MemberFunction value where
  M0 ::
    (FFI value, FFI result) =>
    Constraint constraint ->
    (constraint => value -> result) ->
    MemberFunction value
  M1 ::
    (FFI a, FFI value, FFI result) =>
    Constraint constraint ->
    Text ->
    (constraint => a -> value -> result) ->
    MemberFunction value
  M2 ::
    (FFI a, FFI b, FFI value, FFI result) =>
    Constraint constraint ->
    Text ->
    Text ->
    (constraint => a -> b -> value -> result) ->
    MemberFunction value
  M3 ::
    (FFI a, FFI b, FFI c, FFI value, FFI result) =>
    Constraint constraint ->
    Text ->
    Text ->
    Text ->
    (constraint => a -> b -> c -> value -> result) ->
    MemberFunction value
  M4 ::
    (FFI a, FFI b, FFI c, FFI d, FFI value, FFI result) =>
    Constraint constraint ->
    Text ->
    Text ->
    Text ->
    Text ->
    (constraint => a -> b -> c -> d -> value -> result) ->
    MemberFunction value

callConstructor :: Constructor value -> Ptr () -> Ptr () -> IO ()
callConstructor constructor = case constructor of
  C0 N v ->
    \_ outputPtr -> FFI.store outputPtr 0 v
  C0 F v ->
    \inputPtr outputPtr -> IO.do
      tolerance <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance v)
  C0 L v ->
    \inputPtr outputPtr -> IO.do
      tolerance <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance v)
  C1 N _ f ->
    \inputPtr outputPtr -> IO.do
      arg1 <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (f arg1)
  C1 F _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1))
  C1 L _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1))
  C2 N _ _ f ->
    \inputPtr outputPtr -> IO.do
      (arg1, arg2) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (f arg1 arg2)
  C2 F _ _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2))
  C2 L _ _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2))
  C3 N _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (arg1, arg2, arg3) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (f arg1 arg2 arg3)
  C3 F _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2, arg3) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2 arg3))
  C3 L _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2, arg3) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2 arg3))
  C4 N _ _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (arg1, arg2, arg3, arg4) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (f arg1 arg2 arg3 arg4)
  C4 F _ _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2, arg3, arg4) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2 arg3 arg4))
  C4 L _ _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2, arg3, arg4) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2 arg3 arg4))

callStaticFunction :: StaticFunction -> Ptr () -> Ptr () -> IO ()
callStaticFunction function = case function of
  S0 N v ->
    \_ outputPtr -> FFI.store outputPtr 0 v
  S0 F v ->
    \inputPtr outputPtr -> IO.do
      tolerance <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance v)
  S0 L v ->
    \inputPtr outputPtr -> IO.do
      tolerance <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance v)
  S1 N _ f ->
    \inputPtr outputPtr -> IO.do
      arg1 <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (f arg1)
  S1 F _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1))
  S1 L _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1))
  S2 N _ _ f ->
    \inputPtr outputPtr -> IO.do
      (arg1, arg2) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (f arg1 arg2)
  S2 F _ _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2))
  S2 L _ _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2))
  S3 N _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (arg1, arg2, arg3) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (f arg1 arg2 arg3)
  S3 F _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2, arg3) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2 arg3))
  S3 L _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2, arg3) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2 arg3))
  S4 N _ _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (arg1, arg2, arg3, arg4) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (f arg1 arg2 arg3 arg4)
  S4 F _ _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2, arg3, arg4) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2 arg3 arg4))
  S4 L _ _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2, arg3, arg4) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2 arg3 arg4))

callMemberFunction :: MemberFunction value -> Ptr () -> Ptr () -> IO ()
callMemberFunction function = case function of
  M0 N f ->
    \inputPtr outputPtr -> IO.do
      self <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (f self)
  M0 F f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, self) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f self))
  M0 L f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, self) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f self))
  M1 N _ f ->
    \inputPtr outputPtr -> IO.do
      (arg1, self) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (f arg1 self)
  M1 F _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, self) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 self))
  M1 L _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, self) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 self))
  M2 N _ _ f ->
    \inputPtr outputPtr -> IO.do
      (arg1, arg2, self) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (f arg1 arg2 self)
  M2 F _ _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2, self) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2 self))
  M2 L _ _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2, self) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2 self))
  M3 N _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (arg1, arg2, arg3, self) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (f arg1 arg2 arg3 self)
  M3 F _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2, arg3, self) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2 arg3 self))
  M3 L _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2, arg3, self) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2 arg3 self))
  M4 N _ _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (arg1, arg2, arg3, arg4, self) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (f arg1 arg2 arg3 arg4 self)
  M4 F _ _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2, arg3, arg4, self) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2 arg3 arg4 self))
  M4 L _ _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2, arg3, arg4, self) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2 arg3 arg4 self))
