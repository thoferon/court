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
import Court.Utils

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
    hPutStrLn stderr $ "Building " ++ show job ++ " ..."
    (buildPath, outputPath, processHandle) <- spawnBuild job
    exitCode <- waitForProcess processHandle
    output <- readFile outputPath
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

spawnBuild :: Job -> IO (FilePath, FilePath, ProcessHandle)
spawnBuild job = do
  now <- getCurrentTime
  let buildDir       = "build." ++ formatTime defaultTimeLocale "%Y%m%d%H%M%S" now
      executablePath = jobProjectPath job </> "build"
      buildPath      = jobProjectPath job </> buildDir
      outputPath     = buildPath </> "build.out"
  createDirectory buildPath
  (inR,  inW) <- createPipeHandles
  hClose inW
  stdout' <- openFile outputPath ReadWriteMode
  processHandle <- runProcess
    executablePath (jobArguments job) (Just buildPath) Nothing
     (Just inR) (Just stdout') Nothing
  return (buildPath, outputPath, processHandle)

cleanBuilds :: Int -> FilePath -> IO ()
cleanBuilds n path = do
  items <- getDirectoryContents path
  mapM_ (removeDirectoryRecursive . (path </>))
    . drop n
    . sortBy (\a b -> b `compare` a)
    . filter ("build." `isPrefixOf`)
    $ items
