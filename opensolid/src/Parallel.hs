module Parallel
  ( (<*>)
  , (>>=)
  , (>>)
  , fail
  , return
  , pure
  , join
  , fmap
  )
where

import Control.Concurrent.Async qualified as Async
import Control.Monad (join)
import IO qualified
import OpenSolid hiding (pure, return, (>>))
import Pair qualified
import Prelude (fmap)

pure :: a -> IO a
pure = IO.pure

return :: a -> IO a
return = IO.return

fail :: String -> IO a
fail = IO.fail

(>>=) :: IO.Bind m => m a -> (a -> IO b) -> IO b
(>>=) = (IO.>>=)

(>>) :: IO a -> IO b -> IO b
io1 >> io2 = Prelude.fmap Pair.second (Async.concurrently io1 io2)

(<*>) :: IO (a -> b) -> IO a -> IO b
functionIO <*> valueIO =
  Prelude.fmap (\(function, value) -> function value) (Async.concurrently functionIO valueIO)
