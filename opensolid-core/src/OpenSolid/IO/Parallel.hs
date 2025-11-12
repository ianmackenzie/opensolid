module OpenSolid.IO.Parallel
  ( map2
  , map3
  , map4
  , run
  , collect
  , collectWithIndex
  , forEach
  , forEachWithIndex
  )
where

import Control.Concurrent.Async (Concurrently (Concurrently))
import Control.Concurrent.Async qualified as Async
import Data.Foldable.WithIndex (FoldableWithIndex)
import Data.Foldable.WithIndex qualified
import Data.Traversable.WithIndex (TraversableWithIndex)
import Data.Traversable.WithIndex qualified
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

run :: Foldable list => list (IO ()) -> IO ()
run = Async.mapConcurrently_ id

collect :: Traversable list => (a -> IO b) -> list a -> IO (list b)
collect = Async.mapConcurrently

collectWithIndex :: TraversableWithIndex Int list => (Int -> a -> IO b) -> list a -> IO (list b)
collectWithIndex function list = do
  let task index item = Async.Concurrently (function index item)
  Async.runConcurrently (Data.Traversable.WithIndex.itraverse task list)

forEach :: Foldable list => list a -> (a -> IO ()) -> IO ()
forEach = Async.forConcurrently_

forEachWithIndex :: FoldableWithIndex Int list => list a -> (Int -> a -> IO ()) -> IO ()
forEachWithIndex list function = do
  let task index item = Async.Concurrently (function index item)
  Async.runConcurrently (Data.Foldable.WithIndex.itraverse_ task list)
