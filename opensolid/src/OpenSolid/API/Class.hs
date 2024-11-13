module OpenSolid.API.Class
  ( Class (Class)
  , Function (..)
  , Constraint (..)
  , call
  )
where

import Data.Kind qualified
import Foreign (Ptr)
import IO qualified
import OpenSolid
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import Tolerance qualified
import Units (Meters)

data Class where
  Class :: FFI value => Text -> List (Function value) -> Class

data Constraint (constraint :: Data.Kind.Constraint) where
  N :: Constraint ()
  F :: Constraint (Tolerance Unitless)
  L :: Constraint (Tolerance Meters)

data Function value where
  C0 ::
    FFI value =>
    Constraint constraint ->
    Text ->
    (constraint => value) ->
    Function value
  C1 ::
    (FFI a, FFI value) =>
    Constraint constraint ->
    Text ->
    Text ->
    (constraint => a -> value) ->
    Function value
  C2 ::
    (FFI a, FFI b, FFI value) =>
    Constraint constraint ->
    Text ->
    Text ->
    Text ->
    (constraint => a -> b -> value) ->
    Function value
  C3 ::
    (FFI a, FFI b, FFI c, FFI value) =>
    Constraint constraint ->
    Text ->
    Text ->
    Text ->
    Text ->
    (constraint => a -> b -> c -> value) ->
    Function value
  C4 ::
    (FFI a, FFI b, FFI c, FFI d, FFI value) =>
    Constraint constraint ->
    Text ->
    Text ->
    Text ->
    Text ->
    Text ->
    (constraint => a -> b -> c -> d -> value) ->
    Function value
  F0 ::
    FFI value =>
    Constraint constraint ->
    Text ->
    (constraint => value) ->
    Function value
  F1 ::
    (FFI a, FFI value) =>
    Constraint constraint ->
    Text ->
    Text ->
    (constraint => a -> value) ->
    Function value
  F2 ::
    (FFI a, FFI b, FFI value) =>
    Constraint constraint ->
    Text ->
    Text ->
    Text ->
    (constraint => a -> b -> value) ->
    Function value
  F3 ::
    (FFI a, FFI b, FFI c, FFI value) =>
    Constraint constraint ->
    Text ->
    Text ->
    Text ->
    Text ->
    (constraint => a -> b -> c -> value) ->
    Function value
  F4 ::
    (FFI a, FFI b, FFI c, FFI d, FFI value) =>
    Constraint constraint ->
    Text ->
    Text ->
    Text ->
    Text ->
    Text ->
    (constraint => a -> b -> c -> d -> value) ->
    Function value
  S0 ::
    FFI result =>
    Constraint constraint ->
    Text ->
    (constraint => result) ->
    Function value
  S1 ::
    (FFI a, FFI result) =>
    Constraint constraint ->
    Text ->
    Text ->
    (constraint => a -> result) ->
    Function value
  S2 ::
    (FFI a, FFI b, FFI result) =>
    Constraint constraint ->
    Text ->
    Text ->
    Text ->
    (constraint => a -> b -> result) ->
    Function value
  S3 ::
    (FFI a, FFI b, FFI c, FFI result) =>
    Constraint constraint ->
    Text ->
    Text ->
    Text ->
    Text ->
    (constraint => a -> b -> c -> result) ->
    Function value
  S4 ::
    (FFI a, FFI b, FFI c, FFI d, FFI result) =>
    Constraint constraint ->
    Text ->
    Text ->
    Text ->
    Text ->
    Text ->
    (constraint => a -> b -> c -> d -> result) ->
    Function value
  M0 ::
    (FFI value, FFI result) =>
    Constraint constraint ->
    Text ->
    (constraint => value -> result) ->
    Function value
  M1 ::
    (FFI a, FFI value, FFI result) =>
    Constraint constraint ->
    Text ->
    Text ->
    (constraint => a -> value -> result) ->
    Function value
  M2 ::
    (FFI a, FFI b, FFI value, FFI result) =>
    Constraint constraint ->
    Text ->
    Text ->
    Text ->
    (constraint => a -> b -> value -> result) ->
    Function value
  M3 ::
    (FFI a, FFI b, FFI c, FFI value, FFI result) =>
    Constraint constraint ->
    Text ->
    Text ->
    Text ->
    Text ->
    (constraint => a -> b -> c -> value -> result) ->
    Function value
  M4 ::
    (FFI a, FFI b, FFI c, FFI d, FFI value, FFI result) =>
    Constraint constraint ->
    Text ->
    Text ->
    Text ->
    Text ->
    Text ->
    (constraint => a -> b -> c -> d -> value -> result) ->
    Function value

call :: Function value -> Ptr () -> Ptr () -> IO ()
call function = case function of
  C0 N _ v ->
    \_ outputPtr -> FFI.store outputPtr 0 v
  C0 F _ v ->
    \inputPtr outputPtr -> IO.do
      tolerance <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance v)
  C0 L _ v ->
    \inputPtr outputPtr -> IO.do
      tolerance <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance v)
  C1 N _ _ f ->
    \inputPtr outputPtr -> IO.do
      arg1 <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (f arg1)
  C1 F _ _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1))
  C1 L _ _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1))
  C2 N _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (arg1, arg2) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (f arg1 arg2)
  C2 F _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2))
  C2 L _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2))
  C3 N _ _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (arg1, arg2, arg3) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (f arg1 arg2 arg3)
  C3 F _ _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2, arg3) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2 arg3))
  C3 L _ _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2, arg3) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2 arg3))
  C4 N _ _ _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (arg1, arg2, arg3, arg4) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (f arg1 arg2 arg3 arg4)
  C4 F _ _ _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2, arg3, arg4) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2 arg3 arg4))
  C4 L _ _ _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2, arg3, arg4) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2 arg3 arg4))
  F0 N _ v ->
    \_ outputPtr -> FFI.store outputPtr 0 v
  F0 F _ v ->
    \inputPtr outputPtr -> IO.do
      tolerance <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance v)
  F0 L _ v ->
    \inputPtr outputPtr -> IO.do
      tolerance <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance v)
  F1 N _ _ f ->
    \inputPtr outputPtr -> IO.do
      arg1 <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (f arg1)
  F1 F _ _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1))
  F1 L _ _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1))
  F2 N _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (arg1, arg2) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (f arg1 arg2)
  F2 F _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2))
  F2 L _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2))
  F3 N _ _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (arg1, arg2, arg3) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (f arg1 arg2 arg3)
  F3 F _ _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2, arg3) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2 arg3))
  F3 L _ _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2, arg3) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2 arg3))
  F4 N _ _ _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (arg1, arg2, arg3, arg4) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (f arg1 arg2 arg3 arg4)
  F4 F _ _ _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2, arg3, arg4) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2 arg3 arg4))
  F4 L _ _ _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2, arg3, arg4) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2 arg3 arg4))
  S0 N _ v ->
    \_ outputPtr -> FFI.store outputPtr 0 v
  S0 F _ v ->
    \inputPtr outputPtr -> IO.do
      tolerance <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance v)
  S0 L _ v ->
    \inputPtr outputPtr -> IO.do
      tolerance <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance v)
  S1 N _ _ f ->
    \inputPtr outputPtr -> IO.do
      arg1 <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (f arg1)
  S1 F _ _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1))
  S1 L _ _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1))
  S2 N _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (arg1, arg2) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (f arg1 arg2)
  S2 F _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2))
  S2 L _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2))
  S3 N _ _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (arg1, arg2, arg3) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (f arg1 arg2 arg3)
  S3 F _ _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2, arg3) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2 arg3))
  S3 L _ _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2, arg3) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2 arg3))
  S4 N _ _ _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (arg1, arg2, arg3, arg4) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (f arg1 arg2 arg3 arg4)
  S4 F _ _ _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2, arg3, arg4) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2 arg3 arg4))
  S4 L _ _ _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2, arg3, arg4) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2 arg3 arg4))
  M0 N _ f ->
    \inputPtr outputPtr -> IO.do
      self <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (f self)
  M0 F _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, self) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f self))
  M0 L _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, self) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f self))
  M1 N _ _ f ->
    \inputPtr outputPtr -> IO.do
      (self, arg1) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (f arg1 self)
  M1 F _ _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, self, arg1) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 self))
  M1 L _ _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, self, arg1) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 self))
  M2 N _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (self, arg1, arg2) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (f arg1 arg2 self)
  M2 F _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, self, arg1, arg2) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2 self))
  M2 L _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, self, arg1, arg2) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2 self))
  M3 N _ _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (self, arg1, arg2, arg3) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (f arg1 arg2 arg3 self)
  M3 F _ _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, self, arg1, arg2, arg3) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2 arg3 self))
  M3 L _ _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, self, arg1, arg2, arg3) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2 arg3 self))
  M4 N _ _ _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (self, arg1, arg2, arg3, arg4) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (f arg1 arg2 arg3 arg4 self)
  M4 F _ _ _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, self, arg1, arg2, arg3, arg4) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2 arg3 arg4 self))
  M4 L _ _ _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, self, arg1, arg2, arg3, arg4) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2 arg3 arg4 self))
