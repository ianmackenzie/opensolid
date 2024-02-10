module Try
  ( (>>=)
  , (>>)
  , fail
  , withContext
  , return
  , pure
  , join
  , (<*>)
  , fmap
  )
where

import OpenSolid hiding ((<*>), (>>), (>>=))
import OpenSolid qualified
import Result qualified
import String qualified
import Task qualified
import Prelude (Applicative, Monad)

class MapError m where
  mapError :: (x -> String) -> m x a -> m String a

instance MapError Result where
  mapError = Result.mapError

instance MapError Task where
  mapError = Task.mapError

(>>) ::
  ( MapError m
  , ErrorMessage x
  , Composition (m String a) (m String b) (m String b)
  ) =>
  m x a ->
  m String b ->
  m String b
monad1 >> monad2 = mapError errorMessage monad1 OpenSolid.>> monad2

(<*>) :: (Applicative (m String), MapError m, ErrorMessage x) => m x (a -> b) -> m String a -> m String b
functionApplicative <*> valueApplicative =
  mapError errorMessage functionApplicative OpenSolid.<*> valueApplicative

(>>=) :: (Monad (m String), MapError m, ErrorMessage x) => m x a -> (a -> m String b) -> m String b
monad >>= function =
  mapError errorMessage monad OpenSolid.>>= function

withContext :: (MapError m, ErrorMessage x) => String -> m x a -> m String a
withContext context = mapError (\error -> errorMessage error |> addContext context)

addContext :: String -> String -> String
addContext context text = context ++ ":\n" ++ String.indent "  " text
