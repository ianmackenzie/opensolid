module OpenSolid.IO.Parallel
  ( map2
  , map3
  , map4
  , run
  , collect
  , forEach
  , forEachWithIndex
  )
where

import Control.Concurrent.Async (Concurrently (Concurrently))
import Control.Concurrent.Async qualified as Async
import OpenSolid.List qualified as List
import OpenSolid.Prelude

map2 :: (a -> b -> c) -> IO a -> IO b -> IO c
map2 function taskA taskB =
  Async.runConcurrently $
    function
      <$> Concurrently taskA
      <*> Concurrently taskB

map3 :: (a -> b -> c -> d) -> IO a -> IO b -> IO c -> IO d
map3 function taskA taskB taskC =
  Async.runConcurrently $
    function
      <$> Concurrently taskA
      <*> Concurrently taskB
      <*> Concurrently taskC

map4 :: (a -> b -> c -> d -> e) -> IO a -> IO b -> IO c -> IO d -> IO e
map4 function taskA taskB taskC taskD =
  Async.runConcurrently $
    function
      <$> Concurrently taskA
      <*> Concurrently taskB
      <*> Concurrently taskC
      <*> Concurrently taskD

run :: List (IO ()) -> IO ()
run = Async.mapConcurrently_ id

collect :: Traversable list => (a -> IO b) -> list a -> IO (list b)
collect = Async.mapConcurrently

forEach :: List a -> (a -> IO ()) -> IO ()
forEach list function = run (List.map function list)

forEachWithIndex :: List a -> (Int -> a -> IO ()) -> IO ()
forEachWithIndex list function = run (List.mapWithIndex function list)
