module OpenSolid.API.MemberFunction
  ( MemberFunction (..)
  , ffiName
  , invoke
  , signature
  , documentation
  )
where

import Data.Proxy (Proxy (Proxy))
import Foreign (Ptr)
import OpenSolid.API.Argument qualified as Argument
import OpenSolid.API.ImplicitArgument (ImplicitArgument (..))
import OpenSolid.FFI (FFI, Name)
import OpenSolid.FFI qualified as FFI
import OpenSolid.IO qualified as IO
import OpenSolid.List qualified as List
import OpenSolid.Pair qualified as Pair
import OpenSolid.Prelude
import OpenSolid.Text qualified as Text
import OpenSolid.Tolerance qualified as Tolerance

data MemberFunction where
  MemberFunction0 ::
    (FFI value, FFI result) =>
    (value -> result) ->
    Text ->
    MemberFunction
  MemberFunctionU0 ::
    (FFI value, FFI result) =>
    (Tolerance Unitless => value -> result) ->
    Text ->
    MemberFunction
  MemberFunctionR0 ::
    (FFI value, FFI result) =>
    (Tolerance Radians => value -> result) ->
    Text ->
    MemberFunction
  MemberFunctionM0 ::
    (FFI value, FFI result) =>
    (Tolerance Meters => value -> result) ->
    Text ->
    MemberFunction
  MemberFunctionS0 ::
    (FFI value, FFI result) =>
    (Tolerance SquareMeters => value -> result) ->
    Text ->
    MemberFunction
  MemberFunction1 ::
    (FFI a, FFI value, FFI result) =>
    Name ->
    (a -> value -> result) ->
    Text ->
    MemberFunction
  MemberFunctionU1 ::
    (FFI a, FFI value, FFI result) =>
    Name ->
    (Tolerance Unitless => a -> value -> result) ->
    Text ->
    MemberFunction
  MemberFunctionR1 ::
    (FFI a, FFI value, FFI result) =>
    Name ->
    (Tolerance Radians => a -> value -> result) ->
    Text ->
    MemberFunction
  MemberFunctionM1 ::
    (FFI a, FFI value, FFI result) =>
    Name ->
    (Tolerance Meters => a -> value -> result) ->
    Text ->
    MemberFunction
  MemberFunctionS1 ::
    (FFI a, FFI value, FFI result) =>
    Name ->
    (Tolerance SquareMeters => a -> value -> result) ->
    Text ->
    MemberFunction
  MemberFunction2 ::
    (FFI a, FFI b, FFI value, FFI result) =>
    Name ->
    Name ->
    (a -> b -> value -> result) ->
    Text ->
    MemberFunction
  MemberFunctionU2 ::
    (FFI a, FFI b, FFI value, FFI result) =>
    Name ->
    Name ->
    (Tolerance Unitless => a -> b -> value -> result) ->
    Text ->
    MemberFunction
  MemberFunctionR2 ::
    (FFI a, FFI b, FFI value, FFI result) =>
    Name ->
    Name ->
    (Tolerance Radians => a -> b -> value -> result) ->
    Text ->
    MemberFunction
  MemberFunctionM2 ::
    (FFI a, FFI b, FFI value, FFI result) =>
    Name ->
    Name ->
    (Tolerance Meters => a -> b -> value -> result) ->
    Text ->
    MemberFunction
  MemberFunctionS2 ::
    (FFI a, FFI b, FFI value, FFI result) =>
    Name ->
    Name ->
    (Tolerance SquareMeters => a -> b -> value -> result) ->
    Text ->
    MemberFunction
  MemberFunction3 ::
    (FFI a, FFI b, FFI c, FFI value, FFI result) =>
    Name ->
    Name ->
    Name ->
    (a -> b -> c -> value -> result) ->
    Text ->
    MemberFunction
  MemberFunctionU3 ::
    (FFI a, FFI b, FFI c, FFI value, FFI result) =>
    Name ->
    Name ->
    Name ->
    (Tolerance Unitless => a -> b -> c -> value -> result) ->
    Text ->
    MemberFunction
  MemberFunctionR3 ::
    (FFI a, FFI b, FFI c, FFI value, FFI result) =>
    Name ->
    Name ->
    Name ->
    (Tolerance Radians => a -> b -> c -> value -> result) ->
    Text ->
    MemberFunction
  MemberFunctionM3 ::
    (FFI a, FFI b, FFI c, FFI value, FFI result) =>
    Name ->
    Name ->
    Name ->
    (Tolerance Meters => a -> b -> c -> value -> result) ->
    Text ->
    MemberFunction
  MemberFunctionS3 ::
    (FFI a, FFI b, FFI c, FFI value, FFI result) =>
    Name ->
    Name ->
    Name ->
    (Tolerance SquareMeters => a -> b -> c -> value -> result) ->
    Text ->
    MemberFunction
  MemberFunction4 ::
    (FFI a, FFI b, FFI c, FFI d, FFI value, FFI result) =>
    Name ->
    Name ->
    Name ->
    Name ->
    (a -> b -> c -> d -> value -> result) ->
    Text ->
    MemberFunction
  MemberFunctionU4 ::
    (FFI a, FFI b, FFI c, FFI d, FFI value, FFI result) =>
    Name ->
    Name ->
    Name ->
    Name ->
    (Tolerance Unitless => a -> b -> c -> d -> value -> result) ->
    Text ->
    MemberFunction
  MemberFunctionR4 ::
    (FFI a, FFI b, FFI c, FFI d, FFI value, FFI result) =>
    Name ->
    Name ->
    Name ->
    Name ->
    (Tolerance Radians => a -> b -> c -> d -> value -> result) ->
    Text ->
    MemberFunction
  MemberFunctionM4 ::
    (FFI a, FFI b, FFI c, FFI d, FFI value, FFI result) =>
    Name ->
    Name ->
    Name ->
    Name ->
    (Tolerance Meters => a -> b -> c -> d -> value -> result) ->
    Text ->
    MemberFunction
  MemberFunctionS4 ::
    (FFI a, FFI b, FFI c, FFI d, FFI value, FFI result) =>
    Name ->
    Name ->
    Name ->
    Name ->
    (Tolerance SquareMeters => a -> b -> c -> d -> value -> result) ->
    Text ->
    MemberFunction

ffiName :: FFI.ClassName -> Name -> MemberFunction -> Text
ffiName className functionName memberFunction = do
  let (_, positionalArguments, namedArguments, _) = signature memberFunction
  let arguments = positionalArguments <> namedArguments
  let argumentTypes = List.map Pair.second arguments
  Text.join "_" $
    "opensolid"
      : FFI.concatenatedName className
      : FFI.camelCase functionName
      : List.map FFI.typeName argumentTypes

invoke :: MemberFunction -> Ptr () -> Ptr () -> IO ()
invoke function = case function of
  MemberFunction0 f _ ->
    \inputPtr outputPtr -> IO.do
      self <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (f self)
  MemberFunctionU0 f _ ->
    \inputPtr outputPtr -> IO.do
      (tolerance, self) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f self))
  MemberFunctionR0 f _ ->
    \inputPtr outputPtr -> IO.do
      (tolerance, self) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f self))
  MemberFunctionM0 f _ ->
    \inputPtr outputPtr -> IO.do
      (tolerance, self) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f self))
  MemberFunctionS0 f _ ->
    \inputPtr outputPtr -> IO.do
      (tolerance, self) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f self))
  MemberFunction1 _ f _ ->
    \inputPtr outputPtr -> IO.do
      (arg1, self) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (f arg1 self)
  MemberFunctionU1 _ f _ ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, self) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 self))
  MemberFunctionR1 _ f _ ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, self) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 self))
  MemberFunctionM1 _ f _ ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, self) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 self))
  MemberFunctionS1 _ f _ ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, self) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 self))
  MemberFunction2 _ _ f _ ->
    \inputPtr outputPtr -> IO.do
      (arg1, arg2, self) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (f arg1 arg2 self)
  MemberFunctionU2 _ _ f _ ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2, self) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2 self))
  MemberFunctionR2 _ _ f _ ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2, self) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2 self))
  MemberFunctionM2 _ _ f _ ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2, self) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2 self))
  MemberFunctionS2 _ _ f _ ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2, self) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2 self))
  MemberFunction3 _ _ _ f _ ->
    \inputPtr outputPtr -> IO.do
      (arg1, arg2, arg3, self) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (f arg1 arg2 arg3 self)
  MemberFunctionU3 _ _ _ f _ ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2, arg3, self) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2 arg3 self))
  MemberFunctionR3 _ _ _ f _ ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2, arg3, self) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2 arg3 self))
  MemberFunctionM3 _ _ _ f _ ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2, arg3, self) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2 arg3 self))
  MemberFunctionS3 _ _ _ f _ ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2, arg3, self) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2 arg3 self))
  MemberFunction4 _ _ _ _ f _ ->
    \inputPtr outputPtr -> IO.do
      (arg1, arg2, arg3, arg4, self) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (f arg1 arg2 arg3 arg4 self)
  MemberFunctionU4 _ _ _ _ f _ ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2, arg3, arg4, self) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2 arg3 arg4 self))
  MemberFunctionR4 _ _ _ _ f _ ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2, arg3, arg4, self) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2 arg3 arg4 self))
  MemberFunctionM4 _ _ _ _ f _ ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2, arg3, arg4, self) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2 arg3 arg4 self))
  MemberFunctionS4 _ _ _ _ f _ ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2, arg3, arg4, self) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2 arg3 arg4 self))

type Signature = (Maybe ImplicitArgument, List (Name, FFI.Type, Argument.Kind), FFI.Type)

normalizeSignature ::
  (Maybe ImplicitArgument, List (Name, FFI.Type, Argument.Kind), FFI.Type) ->
  (Maybe ImplicitArgument, List (Name, FFI.Type), List (Name, FFI.Type), FFI.Type)
normalizeSignature (maybeImplicitArgument, arguments, returnType) =
  if not (List.isOrdered (\(_, _, kind1) (_, _, kind2) -> kind1 <= kind2) arguments)
    then internalError "Named arguments should always come after positional arguments"
    else do
      let args desiredKind = [(name, typ) | (name, typ, kind) <- arguments, kind == desiredKind]
      (maybeImplicitArgument, args Argument.Positional, args Argument.Named, returnType)

signature ::
  MemberFunction ->
  (Maybe ImplicitArgument, List (Name, FFI.Type), List (Name, FFI.Type), FFI.Type)
signature memberFunction = normalizeSignature $ case memberFunction of
  MemberFunction0 f _ -> signature0 f
  MemberFunctionU0 f _ -> signatureU0 f
  MemberFunctionR0 f _ -> signatureR0 f
  MemberFunctionM0 f _ -> signatureM0 f
  MemberFunctionS0 f _ -> signatureS0 f
  MemberFunction1 arg1 f _ -> signature1 arg1 f
  MemberFunctionU1 arg1 f _ -> signatureU1 arg1 f
  MemberFunctionR1 arg1 f _ -> signatureR1 arg1 f
  MemberFunctionM1 arg1 f _ -> signatureM1 arg1 f
  MemberFunctionS1 arg1 f _ -> signatureS1 arg1 f
  MemberFunction2 arg1 arg2 f _ -> signature2 arg1 arg2 f
  MemberFunctionU2 arg1 arg2 f _ -> signatureU2 arg1 arg2 f
  MemberFunctionR2 arg1 arg2 f _ -> signatureR2 arg1 arg2 f
  MemberFunctionM2 arg1 arg2 f _ -> signatureM2 arg1 arg2 f
  MemberFunctionS2 arg1 arg2 f _ -> signatureS2 arg1 arg2 f
  MemberFunction3 arg1 arg2 arg3 f _ -> signature3 arg1 arg2 arg3 f
  MemberFunctionU3 arg1 arg2 arg3 f _ -> signatureU3 arg1 arg2 arg3 f
  MemberFunctionR3 arg1 arg2 arg3 f _ -> signatureR3 arg1 arg2 arg3 f
  MemberFunctionM3 arg1 arg2 arg3 f _ -> signatureM3 arg1 arg2 arg3 f
  MemberFunctionS3 arg1 arg2 arg3 f _ -> signatureS3 arg1 arg2 arg3 f
  MemberFunction4 arg1 arg2 arg3 arg4 f _ -> signature4 arg1 arg2 arg3 arg4 f
  MemberFunctionU4 arg1 arg2 arg3 arg4 f _ -> signatureU4 arg1 arg2 arg3 arg4 f
  MemberFunctionR4 arg1 arg2 arg3 arg4 f _ -> signatureR4 arg1 arg2 arg3 arg4 f
  MemberFunctionM4 arg1 arg2 arg3 arg4 f _ -> signatureM4 arg1 arg2 arg3 arg4 f
  MemberFunctionS4 arg1 arg2 arg3 arg4 f _ -> signatureS4 arg1 arg2 arg3 arg4 f

arg :: forall a. FFI a => Name -> Proxy a -> (Name, FFI.Type, Argument.Kind)
arg name proxy = (name, FFI.typeOf proxy, Argument.kind name proxy)

signature0 ::
  forall value result.
  (FFI value, FFI result) =>
  (value -> result) ->
  Signature
signature0 _ = (Nothing, [], FFI.typeOf @result Proxy)

signatureU0 ::
  forall value result.
  (FFI value, FFI result) =>
  (Tolerance Unitless => value -> result) ->
  Signature
signatureU0 _ = (Just ToleranceUnitless, [], FFI.typeOf @result Proxy)

signatureR0 ::
  forall value result.
  (FFI value, FFI result) =>
  (Tolerance Radians => value -> result) ->
  Signature
signatureR0 _ = (Just ToleranceRadians, [], FFI.typeOf @result Proxy)

signatureM0 ::
  forall value result.
  (FFI value, FFI result) =>
  (Tolerance Meters => value -> result) ->
  Signature
signatureM0 _ = (Just ToleranceMeters, [], FFI.typeOf @result Proxy)

signatureS0 ::
  forall value result.
  (FFI value, FFI result) =>
  (Tolerance SquareMeters => value -> result) ->
  Signature
signatureS0 _ = (Just ToleranceSquareMeters, [], FFI.typeOf @result Proxy)

signature1 ::
  forall a value result.
  (FFI a, FFI value, FFI result) =>
  Name ->
  (a -> value -> result) ->
  Signature
signature1 arg1 _ =
  ( Nothing
  , [arg @a arg1 Proxy]
  , FFI.typeOf @result Proxy
  )

signatureU1 ::
  forall a value result.
  (FFI a, FFI value, FFI result) =>
  Name ->
  (Tolerance Unitless => a -> value -> result) ->
  Signature
signatureU1 arg1 _ =
  ( Just ToleranceUnitless
  , [arg @a arg1 Proxy]
  , FFI.typeOf @result Proxy
  )

signatureR1 ::
  forall a value result.
  (FFI a, FFI value, FFI result) =>
  Name ->
  (Tolerance Radians => a -> value -> result) ->
  Signature
signatureR1 arg1 _ =
  ( Just ToleranceRadians
  , [arg @a arg1 Proxy]
  , FFI.typeOf @result Proxy
  )

signatureM1 ::
  forall a value result.
  (FFI a, FFI value, FFI result) =>
  Name ->
  (Tolerance Meters => a -> value -> result) ->
  Signature
signatureM1 arg1 _ =
  ( Just ToleranceMeters
  , [arg @a arg1 Proxy]
  , FFI.typeOf @result Proxy
  )

signatureS1 ::
  forall a value result.
  (FFI a, FFI value, FFI result) =>
  Name ->
  (Tolerance SquareMeters => a -> value -> result) ->
  Signature
signatureS1 arg1 _ =
  ( Just ToleranceSquareMeters
  , [arg @a arg1 Proxy]
  , FFI.typeOf @result Proxy
  )

signature2 ::
  forall a b value result.
  (FFI a, FFI b, FFI value, FFI result) =>
  Name ->
  Name ->
  (a -> b -> value -> result) ->
  Signature
signature2 arg1 arg2 _ =
  ( Nothing
  ,
    [ arg @a arg1 Proxy
    , arg @b arg2 Proxy
    ]
  , FFI.typeOf @result Proxy
  )

signatureU2 ::
  forall a b value result.
  (FFI a, FFI b, FFI value, FFI result) =>
  Name ->
  Name ->
  (Tolerance Unitless => a -> b -> value -> result) ->
  Signature
signatureU2 arg1 arg2 _ =
  ( Just ToleranceUnitless
  ,
    [ arg @a arg1 Proxy
    , arg @b arg2 Proxy
    ]
  , FFI.typeOf @result Proxy
  )

signatureR2 ::
  forall a b value result.
  (FFI a, FFI b, FFI value, FFI result) =>
  Name ->
  Name ->
  (Tolerance Radians => a -> b -> value -> result) ->
  Signature
signatureR2 arg1 arg2 _ =
  ( Just ToleranceRadians
  ,
    [ arg @a arg1 Proxy
    , arg @b arg2 Proxy
    ]
  , FFI.typeOf @result Proxy
  )

signatureM2 ::
  forall a b value result.
  (FFI a, FFI b, FFI value, FFI result) =>
  Name ->
  Name ->
  (Tolerance Meters => a -> b -> value -> result) ->
  Signature
signatureM2 arg1 arg2 _ =
  ( Just ToleranceMeters
  ,
    [ arg @a arg1 Proxy
    , arg @b arg2 Proxy
    ]
  , FFI.typeOf @result Proxy
  )

signatureS2 ::
  forall a b value result.
  (FFI a, FFI b, FFI value, FFI result) =>
  Name ->
  Name ->
  (Tolerance SquareMeters => a -> b -> value -> result) ->
  Signature
signatureS2 arg1 arg2 _ =
  ( Just ToleranceSquareMeters
  ,
    [ arg @a arg1 Proxy
    , arg @b arg2 Proxy
    ]
  , FFI.typeOf @result Proxy
  )

signature3 ::
  forall a b c value result.
  (FFI a, FFI b, FFI c, FFI value, FFI result) =>
  Name ->
  Name ->
  Name ->
  (a -> b -> c -> value -> result) ->
  Signature
signature3 arg1 arg2 arg3 _ =
  ( Nothing
  ,
    [ arg @a arg1 Proxy
    , arg @b arg2 Proxy
    , arg @c arg3 Proxy
    ]
  , FFI.typeOf @result Proxy
  )

signatureU3 ::
  forall a b c value result.
  (FFI a, FFI b, FFI c, FFI value, FFI result) =>
  Name ->
  Name ->
  Name ->
  (Tolerance Unitless => a -> b -> c -> value -> result) ->
  Signature
signatureU3 arg1 arg2 arg3 _ =
  ( Just ToleranceUnitless
  ,
    [ arg @a arg1 Proxy
    , arg @b arg2 Proxy
    , arg @c arg3 Proxy
    ]
  , FFI.typeOf @result Proxy
  )

signatureR3 ::
  forall a b c value result.
  (FFI a, FFI b, FFI c, FFI value, FFI result) =>
  Name ->
  Name ->
  Name ->
  (Tolerance Radians => a -> b -> c -> value -> result) ->
  Signature
signatureR3 arg1 arg2 arg3 _ =
  ( Just ToleranceRadians
  ,
    [ arg @a arg1 Proxy
    , arg @b arg2 Proxy
    , arg @c arg3 Proxy
    ]
  , FFI.typeOf @result Proxy
  )

signatureM3 ::
  forall a b c value result.
  (FFI a, FFI b, FFI c, FFI value, FFI result) =>
  Name ->
  Name ->
  Name ->
  (Tolerance Meters => a -> b -> c -> value -> result) ->
  Signature
signatureM3 arg1 arg2 arg3 _ =
  ( Just ToleranceMeters
  ,
    [ arg @a arg1 Proxy
    , arg @b arg2 Proxy
    , arg @c arg3 Proxy
    ]
  , FFI.typeOf @result Proxy
  )

signatureS3 ::
  forall a b c value result.
  (FFI a, FFI b, FFI c, FFI value, FFI result) =>
  Name ->
  Name ->
  Name ->
  (Tolerance SquareMeters => a -> b -> c -> value -> result) ->
  Signature
signatureS3 arg1 arg2 arg3 _ =
  ( Just ToleranceSquareMeters
  ,
    [ arg @a arg1 Proxy
    , arg @b arg2 Proxy
    , arg @c arg3 Proxy
    ]
  , FFI.typeOf @result Proxy
  )

signature4 ::
  forall a b c d value result.
  (FFI a, FFI b, FFI c, FFI d, FFI value, FFI result) =>
  Name ->
  Name ->
  Name ->
  Name ->
  (a -> b -> c -> d -> value -> result) ->
  Signature
signature4 arg1 arg2 arg3 arg4 _ =
  ( Nothing
  ,
    [ arg @a arg1 Proxy
    , arg @b arg2 Proxy
    , arg @c arg3 Proxy
    , arg @d arg4 Proxy
    ]
  , FFI.typeOf @result Proxy
  )

signatureU4 ::
  forall a b c d value result.
  (FFI a, FFI b, FFI c, FFI d, FFI value, FFI result) =>
  Name ->
  Name ->
  Name ->
  Name ->
  (Tolerance Unitless => a -> b -> c -> d -> value -> result) ->
  Signature
signatureU4 arg1 arg2 arg3 arg4 _ =
  ( Just ToleranceUnitless
  ,
    [ arg @a arg1 Proxy
    , arg @b arg2 Proxy
    , arg @c arg3 Proxy
    , arg @d arg4 Proxy
    ]
  , FFI.typeOf @result Proxy
  )

signatureR4 ::
  forall a b c d value result.
  (FFI a, FFI b, FFI c, FFI d, FFI value, FFI result) =>
  Name ->
  Name ->
  Name ->
  Name ->
  (Tolerance Radians => a -> b -> c -> d -> value -> result) ->
  Signature
signatureR4 arg1 arg2 arg3 arg4 _ =
  ( Just ToleranceRadians
  ,
    [ arg @a arg1 Proxy
    , arg @b arg2 Proxy
    , arg @c arg3 Proxy
    , arg @d arg4 Proxy
    ]
  , FFI.typeOf @result Proxy
  )

signatureM4 ::
  forall a b c d value result.
  (FFI a, FFI b, FFI c, FFI d, FFI value, FFI result) =>
  Name ->
  Name ->
  Name ->
  Name ->
  (Tolerance Meters => a -> b -> c -> d -> value -> result) ->
  Signature
signatureM4 arg1 arg2 arg3 arg4 _ =
  ( Just ToleranceMeters
  ,
    [ arg @a arg1 Proxy
    , arg @b arg2 Proxy
    , arg @c arg3 Proxy
    , arg @d arg4 Proxy
    ]
  , FFI.typeOf @result Proxy
  )

signatureS4 ::
  forall a b c d value result.
  (FFI a, FFI b, FFI c, FFI d, FFI value, FFI result) =>
  Name ->
  Name ->
  Name ->
  Name ->
  (Tolerance SquareMeters => a -> b -> c -> d -> value -> result) ->
  Signature
signatureS4 arg1 arg2 arg3 arg4 _ =
  ( Just ToleranceSquareMeters
  ,
    [ arg @a arg1 Proxy
    , arg @b arg2 Proxy
    , arg @c arg3 Proxy
    , arg @d arg4 Proxy
    ]
  , FFI.typeOf @result Proxy
  )

documentation :: MemberFunction -> Text
documentation memberFunction = case memberFunction of
  MemberFunction0 _ docs -> docs
  MemberFunctionU0 _ docs -> docs
  MemberFunctionR0 _ docs -> docs
  MemberFunctionM0 _ docs -> docs
  MemberFunctionS0 _ docs -> docs
  MemberFunction1 _ _ docs -> docs
  MemberFunctionU1 _ _ docs -> docs
  MemberFunctionR1 _ _ docs -> docs
  MemberFunctionM1 _ _ docs -> docs
  MemberFunctionS1 _ _ docs -> docs
  MemberFunction2 _ _ _ docs -> docs
  MemberFunctionU2 _ _ _ docs -> docs
  MemberFunctionR2 _ _ _ docs -> docs
  MemberFunctionM2 _ _ _ docs -> docs
  MemberFunctionS2 _ _ _ docs -> docs
  MemberFunction3 _ _ _ _ docs -> docs
  MemberFunctionU3 _ _ _ _ docs -> docs
  MemberFunctionR3 _ _ _ _ docs -> docs
  MemberFunctionM3 _ _ _ _ docs -> docs
  MemberFunctionS3 _ _ _ _ docs -> docs
  MemberFunction4 _ _ _ _ _ docs -> docs
  MemberFunctionU4 _ _ _ _ _ docs -> docs
  MemberFunctionR4 _ _ _ _ _ docs -> docs
  MemberFunctionM4 _ _ _ _ _ docs -> docs
  MemberFunctionS4 _ _ _ _ _ docs -> docs
