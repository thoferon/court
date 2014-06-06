{-# LANGUAGE BangPatterns #-}

module Court.Queue
  ( module Control.Concurrent.STM
  , module Data.Monoid
  , Queue(..)
  , takeNextJobSTM
  , takeNextJob
  , addToQueueSTM
  , addToQueue
  ) where

import Control.Concurrent.STM

import Data.List
import Data.Monoid

import Court.Job

data Queue = Queue ![Job] deriving (Show, Eq)

instance Monoid Queue where
  mempty = Queue []
  mappend (Queue xs) (Queue ys) = Queue . nub $ xs ++ ys

takeNextJobSTM :: TVar Queue -> STM (Maybe Job)
takeNextJobSTM queueTVar = do
  Queue jobs <- readTVar queueTVar
  case jobs of
    job : rest -> do
      writeTVar queueTVar $ Queue rest
      return $ Just job
    _ -> return Nothing

takeNextJob :: TVar Queue -> IO (Maybe Job)
takeNextJob = atomically . takeNextJobSTM

addToQueueSTM :: TVar Queue -> Job -> STM ()
addToQueueSTM queueTVar job = modifyTVar queueTVar (<> Queue [job])

addToQueue :: TVar Queue -> Job -> IO ()
addToQueue queueTVar job = atomically $ addToQueueSTM queueTVar job
