module API.StaticFunction
  ( StaticFunction (..)
  , ffiName
  , invoke
  , signature
  , documentation
  )
where

import API.Argument qualified as Argument
import API.Constraint (Constraint (..))
import Data.Proxy (Proxy (Proxy))
import Foreign (Ptr)
import OpenSolid.FFI (FFI, Name)
import OpenSolid.FFI qualified as FFI
import OpenSolid.IO qualified as IO
import OpenSolid.List qualified as List
import OpenSolid.Pair qualified as Pair
import OpenSolid.Prelude
import OpenSolid.Text qualified as Text
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Units (Meters, Radians)

data StaticFunction where
  StaticFunction1 ::
    (FFI a, FFI b) =>
    Name ->
    (a -> b) ->
    Text ->
    StaticFunction
  StaticFunctionU1 ::
    (FFI a, FFI b) =>
    Name ->
    (Tolerance Unitless => a -> b) ->
    Text ->
    StaticFunction
  StaticFunctionR1 ::
    (FFI a, FFI b) =>
    Name ->
    (Tolerance Radians => a -> b) ->
    Text ->
    StaticFunction
  StaticFunctionM1 ::
    (FFI a, FFI b) =>
    Name ->
    (Tolerance Meters => a -> b) ->
    Text ->
    StaticFunction
  StaticFunction2 ::
    (FFI a, FFI b, FFI c) =>
    Name ->
    Name ->
    (a -> b -> c) ->
    Text ->
    StaticFunction
  StaticFunctionU2 ::
    (FFI a, FFI b, FFI c) =>
    Name ->
    Name ->
    (Tolerance Unitless => a -> b -> c) ->
    Text ->
    StaticFunction
  StaticFunctionR2 ::
    (FFI a, FFI b, FFI c) =>
    Name ->
    Name ->
    (Tolerance Radians => a -> b -> c) ->
    Text ->
    StaticFunction
  StaticFunctionM2 ::
    (FFI a, FFI b, FFI c) =>
    Name ->
    Name ->
    (Tolerance Meters => a -> b -> c) ->
    Text ->
    StaticFunction
  StaticFunction3 ::
    (FFI a, FFI b, FFI c, FFI d) =>
    Name ->
    Name ->
    Name ->
    (a -> b -> c -> d) ->
    Text ->
    StaticFunction
  StaticFunctionU3 ::
    (FFI a, FFI b, FFI c, FFI d) =>
    Name ->
    Name ->
    Name ->
    (Tolerance Unitless => a -> b -> c -> d) ->
    Text ->
    StaticFunction
  StaticFunctionR3 ::
    (FFI a, FFI b, FFI c, FFI d) =>
    Name ->
    Name ->
    Name ->
    (Tolerance Radians => a -> b -> c -> d) ->
    Text ->
    StaticFunction
  StaticFunctionM3 ::
    (FFI a, FFI b, FFI c, FFI d) =>
    Name ->
    Name ->
    Name ->
    (Tolerance Meters => a -> b -> c -> d) ->
    Text ->
    StaticFunction
  StaticFunction4 ::
    (FFI a, FFI b, FFI c, FFI d, FFI e) =>
    Name ->
    Name ->
    Name ->
    Name ->
    (a -> b -> c -> d -> e) ->
    Text ->
    StaticFunction
  StaticFunctionU4 ::
    (FFI a, FFI b, FFI c, FFI d, FFI e) =>
    Name ->
    Name ->
    Name ->
    Name ->
    (Tolerance Unitless => a -> b -> c -> d -> e) ->
    Text ->
    StaticFunction
  StaticFunctionR4 ::
    (FFI a, FFI b, FFI c, FFI d, FFI e) =>
    Name ->
    Name ->
    Name ->
    Name ->
    (Tolerance Radians => a -> b -> c -> d -> e) ->
    Text ->
    StaticFunction
  StaticFunctionM4 ::
    (FFI a, FFI b, FFI c, FFI d, FFI e) =>
    Name ->
    Name ->
    Name ->
    Name ->
    (Tolerance Meters => a -> b -> c -> d -> e) ->
    Text ->
    StaticFunction

ffiName :: FFI.Id a -> Name -> StaticFunction -> Text
ffiName classId functionName staticFunction = do
  let (_, positionalArguments, namedArguments, _) = signature staticFunction
  let arguments = positionalArguments <> namedArguments
  let argumentTypes = List.map Pair.second arguments
  Text.join "_" $
    "opensolid"
      : FFI.className classId
      : FFI.camelCase functionName
      : List.map FFI.typeName argumentTypes

invoke :: StaticFunction -> Ptr () -> Ptr () -> IO ()
invoke function = case function of
  StaticFunction1 _ f _ ->
    \inputPtr outputPtr -> IO.do
      arg1 <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (f arg1)
  StaticFunctionU1 _ f _ ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1))
  StaticFunctionR1 _ f _ ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1))
  StaticFunctionM1 _ f _ ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1))
  StaticFunction2 _ _ f _ ->
    \inputPtr outputPtr -> IO.do
      (arg1, arg2) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (f arg1 arg2)
  StaticFunctionU2 _ _ f _ ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2))
  StaticFunctionR2 _ _ f _ ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2))
  StaticFunctionM2 _ _ f _ ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2))
  StaticFunction3 _ _ _ f _ ->
    \inputPtr outputPtr -> IO.do
      (arg1, arg2, arg3) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (f arg1 arg2 arg3)
  StaticFunctionU3 _ _ _ f _ ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2, arg3) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2 arg3))
  StaticFunctionR3 _ _ _ f _ ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2, arg3) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2 arg3))
  StaticFunctionM3 _ _ _ f _ ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2, arg3) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2 arg3))
  StaticFunction4 _ _ _ _ f _ ->
    \inputPtr outputPtr -> IO.do
      (arg1, arg2, arg3, arg4) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (f arg1 arg2 arg3 arg4)
  StaticFunctionU4 _ _ _ _ f _ ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2, arg3, arg4) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2 arg3 arg4))
  StaticFunctionR4 _ _ _ _ f _ ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2, arg3, arg4) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2 arg3 arg4))
  StaticFunctionM4 _ _ _ _ f _ ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2, arg3, arg4) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2 arg3 arg4))

type Signature = (Maybe Constraint, List (Name, FFI.Type, Argument.Kind), FFI.Type)

normalizeSignature ::
  (Maybe Constraint, List (Name, FFI.Type, Argument.Kind), FFI.Type) ->
  (Maybe Constraint, List (Name, FFI.Type), List (Name, FFI.Type), FFI.Type)
normalizeSignature (maybeConstraint, arguments, returnType) =
  if not (List.isOrdered (\(_, _, kind1) (_, _, kind2) -> kind1 <= kind2) arguments)
    then internalError "Named arguments should always come after positional arguments"
    else do
      let args desiredKind = [(name, typ) | (name, typ, kind) <- arguments, kind == desiredKind]
      (maybeConstraint, args Argument.Positional, args Argument.Named, returnType)

signature ::
  StaticFunction ->
  (Maybe Constraint, List (Name, FFI.Type), List (Name, FFI.Type), FFI.Type)
signature staticFunction = normalizeSignature $ case staticFunction of
  StaticFunction1 arg1 f _ -> signature1 arg1 f
  StaticFunctionU1 arg1 f _ -> signatureU1 arg1 f
  StaticFunctionR1 arg1 f _ -> signatureR1 arg1 f
  StaticFunctionM1 arg1 f _ -> signatureM1 arg1 f
  StaticFunction2 arg1 arg2 f _ -> signature2 arg1 arg2 f
  StaticFunctionU2 arg1 arg2 f _ -> signatureU2 arg1 arg2 f
  StaticFunctionR2 arg1 arg2 f _ -> signatureR2 arg1 arg2 f
  StaticFunctionM2 arg1 arg2 f _ -> signatureM2 arg1 arg2 f
  StaticFunction3 arg1 arg2 arg3 f _ -> signature3 arg1 arg2 arg3 f
  StaticFunctionU3 arg1 arg2 arg3 f _ -> signatureU3 arg1 arg2 arg3 f
  StaticFunctionR3 arg1 arg2 arg3 f _ -> signatureR3 arg1 arg2 arg3 f
  StaticFunctionM3 arg1 arg2 arg3 f _ -> signatureM3 arg1 arg2 arg3 f
  StaticFunction4 arg1 arg2 arg3 arg4 f _ -> signature4 arg1 arg2 arg3 arg4 f
  StaticFunctionU4 arg1 arg2 arg3 arg4 f _ -> signatureU4 arg1 arg2 arg3 arg4 f
  StaticFunctionR4 arg1 arg2 arg3 arg4 f _ -> signatureR4 arg1 arg2 arg3 arg4 f
  StaticFunctionM4 arg1 arg2 arg3 arg4 f _ -> signatureM4 arg1 arg2 arg3 arg4 f

arg :: forall a. FFI a => Name -> Proxy a -> (Name, FFI.Type, Argument.Kind)
arg name proxy = (name, FFI.typeOf proxy, Argument.kind name proxy)

signature1 ::
  forall a b.
  (FFI a, FFI b) =>
  Name ->
  (a -> b) ->
  Signature
signature1 arg1 _ = (Nothing, [arg @a arg1 Proxy], FFI.typeOf @b Proxy)

signatureU1 ::
  forall a b.
  (FFI a, FFI b) =>
  Name ->
  (Tolerance Unitless => a -> b) ->
  Signature
signatureU1 arg1 _ = (Just ToleranceUnitless, [arg @a arg1 Proxy], FFI.typeOf @b Proxy)

signatureR1 ::
  forall a b.
  (FFI a, FFI b) =>
  Name ->
  (Tolerance Radians => a -> b) ->
  Signature
signatureR1 arg1 _ = (Just ToleranceRadians, [arg @a arg1 Proxy], FFI.typeOf @b Proxy)

signatureM1 ::
  forall a b.
  (FFI a, FFI b) =>
  Name ->
  (Tolerance Meters => a -> b) ->
  Signature
signatureM1 arg1 _ = (Just ToleranceMeters, [arg @a arg1 Proxy], FFI.typeOf @b Proxy)

signature2 ::
  forall a b c.
  (FFI a, FFI b, FFI c) =>
  Name ->
  Name ->
  (a -> b -> c) ->
  Signature
signature2 arg1 arg2 _ =
  (Nothing, [arg @a arg1 Proxy, arg @b arg2 Proxy], FFI.typeOf @c Proxy)

signatureU2 ::
  forall a b c.
  (FFI a, FFI b, FFI c) =>
  Name ->
  Name ->
  (Tolerance Unitless => a -> b -> c) ->
  Signature
signatureU2 arg1 arg2 _ =
  (Just ToleranceUnitless, [arg @a arg1 Proxy, arg @b arg2 Proxy], FFI.typeOf @c Proxy)

signatureR2 ::
  forall a b c.
  (FFI a, FFI b, FFI c) =>
  Name ->
  Name ->
  (Tolerance Radians => a -> b -> c) ->
  Signature
signatureR2 arg1 arg2 _ =
  (Just ToleranceRadians, [arg @a arg1 Proxy, arg @b arg2 Proxy], FFI.typeOf @c Proxy)

signatureM2 ::
  forall a b c.
  (FFI a, FFI b, FFI c) =>
  Name ->
  Name ->
  (Tolerance Meters => a -> b -> c) ->
  Signature
signatureM2 arg1 arg2 _ =
  (Just ToleranceMeters, [arg @a arg1 Proxy, arg @b arg2 Proxy], FFI.typeOf @c Proxy)

signature3 ::
  forall a b c d.
  (FFI a, FFI b, FFI c, FFI d) =>
  Name ->
  Name ->
  Name ->
  (a -> b -> c -> d) ->
  Signature
signature3 arg1 arg2 arg3 _ =
  ( Nothing
  , [arg @a arg1 Proxy, arg @b arg2 Proxy, arg @c arg3 Proxy]
  , FFI.typeOf @d Proxy
  )

signatureU3 ::
  forall a b c d.
  (FFI a, FFI b, FFI c, FFI d) =>
  Name ->
  Name ->
  Name ->
  (Tolerance Unitless => a -> b -> c -> d) ->
  Signature
signatureU3 arg1 arg2 arg3 _ =
  ( Just ToleranceUnitless
  , [arg @a arg1 Proxy, arg @b arg2 Proxy, arg @c arg3 Proxy]
  , FFI.typeOf @d Proxy
  )

signatureR3 ::
  forall a b c d.
  (FFI a, FFI b, FFI c, FFI d) =>
  Name ->
  Name ->
  Name ->
  (Tolerance Radians => a -> b -> c -> d) ->
  Signature
signatureR3 arg1 arg2 arg3 _ =
  ( Just ToleranceRadians
  , [arg @a arg1 Proxy, arg @b arg2 Proxy, arg @c arg3 Proxy]
  , FFI.typeOf @d Proxy
  )

signatureM3 ::
  forall a b c d.
  (FFI a, FFI b, FFI c, FFI d) =>
  Name ->
  Name ->
  Name ->
  (Tolerance Meters => a -> b -> c -> d) ->
  Signature
signatureM3 arg1 arg2 arg3 _ =
  ( Just ToleranceMeters
  , [arg @a arg1 Proxy, arg @b arg2 Proxy, arg @c arg3 Proxy]
  , FFI.typeOf @d Proxy
  )

signature4 ::
  forall a b c d e.
  (FFI a, FFI b, FFI c, FFI d, FFI e) =>
  Name ->
  Name ->
  Name ->
  Name ->
  (a -> b -> c -> d -> e) ->
  Signature
signature4 arg1 arg2 arg3 arg4 _ =
  ( Nothing
  , [arg @a arg1 Proxy, arg @b arg2 Proxy, arg @c arg3 Proxy, arg @d arg4 Proxy]
  , FFI.typeOf @e Proxy
  )

signatureU4 ::
  forall a b c d e.
  (FFI a, FFI b, FFI c, FFI d, FFI e) =>
  Name ->
  Name ->
  Name ->
  Name ->
  (Tolerance Unitless => a -> b -> c -> d -> e) ->
  Signature
signatureU4 arg1 arg2 arg3 arg4 _ =
  ( Just ToleranceUnitless
  , [arg @a arg1 Proxy, arg @b arg2 Proxy, arg @c arg3 Proxy, arg @d arg4 Proxy]
  , FFI.typeOf @e Proxy
  )

signatureR4 ::
  forall a b c d e.
  (FFI a, FFI b, FFI c, FFI d, FFI e) =>
  Name ->
  Name ->
  Name ->
  Name ->
  (Tolerance Radians => a -> b -> c -> d -> e) ->
  Signature
signatureR4 arg1 arg2 arg3 arg4 _ =
  ( Just ToleranceRadians
  , [arg @a arg1 Proxy, arg @b arg2 Proxy, arg @c arg3 Proxy, arg @d arg4 Proxy]
  , FFI.typeOf @e Proxy
  )

signatureM4 ::
  forall a b c d e.
  (FFI a, FFI b, FFI c, FFI d, FFI e) =>
  Name ->
  Name ->
  Name ->
  Name ->
  (Tolerance Meters => a -> b -> c -> d -> e) ->
  Signature
signatureM4 arg1 arg2 arg3 arg4 _ =
  ( Just ToleranceMeters
  , [arg @a arg1 Proxy, arg @b arg2 Proxy, arg @c arg3 Proxy, arg @d arg4 Proxy]
  , FFI.typeOf @e Proxy
  )

documentation :: StaticFunction -> Text
documentation memberFunction = case memberFunction of
  StaticFunction1 _ _ docs -> docs
  StaticFunctionU1 _ _ docs -> docs
  StaticFunctionR1 _ _ docs -> docs
  StaticFunctionM1 _ _ docs -> docs
  StaticFunction2 _ _ _ docs -> docs
  StaticFunctionU2 _ _ _ docs -> docs
  StaticFunctionR2 _ _ _ docs -> docs
  StaticFunctionM2 _ _ _ docs -> docs
  StaticFunction3 _ _ _ _ docs -> docs
  StaticFunctionU3 _ _ _ _ docs -> docs
  StaticFunctionR3 _ _ _ _ docs -> docs
  StaticFunctionM3 _ _ _ _ docs -> docs
  StaticFunction4 _ _ _ _ _ docs -> docs
  StaticFunctionU4 _ _ _ _ _ docs -> docs
  StaticFunctionR4 _ _ _ _ _ docs -> docs
  StaticFunctionM4 _ _ _ _ _ docs -> docs
