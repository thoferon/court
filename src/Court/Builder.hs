{-# LANGUAGE ScopedTypeVariables #-}

module Court.Builder
  ( builder
  , buildNext
  ) where

import Control.Concurrent
import Control.Exception
import Control.Monad

import Data.List
import Data.Maybe
import Data.Time

import System.Directory
import System.FilePath
import System.IO
import System.Locale
import System.Process

import Court.Job
import Court.Options
import Court.Queue
import Court.Result

builder :: Options -> TVar Queue -> IO ()
builder opts queueTVar = builder' []
  where
    builder' :: [MVar ()] -> IO ()
    builder' mvars = do
      mJob   <- takeNextJob queueTVar
      mvars' <- case mJob of
        Nothing  -> do
          threadDelay $ 1000 * 1000
          return mvars
        Just job -> builder'' job mvars
      builder' mvars'

    builder'' :: Job -> [MVar ()] -> IO [MVar ()]
    builder'' job mvars = do
      mvars' <- cleanMVars mvars
      if length mvars' >= optThreads opts
        then do
          threadDelay $ 5 * 1000 * 1000
          builder'' job mvars'
        else do
          mvar' <- newEmptyMVar
          _ <- forkIO $ buildNext opts job mvar'
          return $ mvar' : mvars'

    cleanMVars :: [MVar ()] -> IO [MVar ()]
    cleanMVars mvars = do
      mMVars <- forM mvars $ \mvar -> do
        mres <- tryTakeMVar mvar
        return $ maybe (Just mvar) (const Nothing) mres
      return $ catMaybes mMVars

buildNext :: Options -> Job -> MVar () -> IO ()
buildNext opts job mvar = handle errorHandler $ do
    hPutStrLn stderr $ "Building " ++ jobProjectPath job ++ " ..."
    (buildPath, stdout', processHandle) <- spawnBuild job
    exitCode <- waitForProcess processHandle
    output   <- hGetContents stdout'
    let result = Result
          { resultExitCode = exitCode
          , resultOutput   = output
          , resultPath     = buildPath
          }
    modifyGlobalResults opts $ changeResults result
    modifyLocalResults  job  $ changeResults result
    cleanBuilds 20 $ jobProjectPath job
    putMVar mvar ()
  where
    changeResults :: Result -> Results -> Results
    changeResults result (Results results) = Results $ take 20 $ result : results

    errorHandler :: SomeException -> IO ()
    errorHandler e = do
      hPutStrLn stderr $ "ERROR: " ++ show e
      putMVar mvar ()

spawnBuild :: Job -> IO (FilePath, Handle, ProcessHandle)
spawnBuild job = do
  now <- getCurrentTime
  let buildDir       = "build." ++ formatTime defaultTimeLocale "%Y%m%d%H%M%S" now
      executablePath = jobProjectPath job </> "build"
      buildPath      = jobProjectPath job </> buildDir
  createDirectory buildPath
  (stdin', stdout', stderr', processHandle) <-
    runInteractiveProcess executablePath (jobArguments job) (Just buildPath) Nothing
  hClose stdin'
  hClose stderr'
  return (buildPath, stdout', processHandle)

cleanBuilds :: Int -> FilePath -> IO ()
cleanBuilds n path = do
  items <- getDirectoryContents path
  mapM_ (removeDirectoryRecursive . (path </>))
    . drop n
    . sortBy (\a b -> b `compare` a)
    . filter ("build." `isPrefixOf`)
    $ items
