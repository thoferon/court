{-# LANGUAGE ScopedTypeVariables #-}

module Court.PluginManager
  ( pluginManager
  ) where

import Control.Concurrent
import Control.Exception
import Control.Monad

import Data.List

import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.Process

import Court.Job
import Court.Options
import Court.Plugin
import Court.Queue
import Court.Utils

pluginManager :: Options -> TVar Queue -> IO ()
pluginManager opts queueTVar = do
  pluginMap <- loadPluginMapOrDie $ optPluginMapPath opts
  basePath <- if isAbsolute (optDirectory opts)
    then return $ optDirectory opts
    else do
      current <- getCurrentDirectory
      return $ normalise $ current </> optDirectory opts
  let pairs = pluginMapToPairs basePath pluginMap
  runningPlugins <- mapM (uncurry spawnPlugin) pairs
  consumePlugins queueTVar runningPlugins

loadPluginMapOrDie :: FilePath -> IO PluginMap
loadPluginMapOrDie path = do
  contents <- readFile path
  case reads contents of
    (pluginMap,_):_ -> return pluginMap
    _ -> hPutStrLn stderr ("Can't read plugin map at " ++ path) >> exitFailure

spawnPlugin :: FilePath -> Plugin -> IO RunningPlugin
spawnPlugin path plugin = do
  hPutStrLn stderr $ "Spawning " ++ show plugin ++ " in " ++ path ++ " ..."
  let args = pluginArguments plugin
  (inR,  inW)   <- createPipeHandles
  (outR, outW)  <- createPipeHandles
  processHandle <- runProcess
    (pluginExecutable plugin) args (Just path) Nothing
    (Just inR) (Just outW) Nothing
  hClose inW
  return RunningPlugin
    { runningPluginStdout  = outR
    , runningPluginProcess = processHandle
    , runningPluginOrigin  = (path, plugin)
    }

consumePlugins :: TVar Queue -> [RunningPlugin] -> IO ()
consumePlugins queueTVar runningPlugins = do
    runningPlugins' <- forM runningPlugins $ \runningPlugin -> do
      let kill    = terminateProcess $ runningPluginProcess runningPlugin
          respawn = uncurry spawnPlugin $ runningPluginOrigin runningPlugin

      handle (\(_ :: SomeException) -> kill >> respawn) $ do
        let out = runningPluginStdout runningPlugin
        checkReady <- hReady out
        when checkReady $ do
          line <- hGetLine out
          let job = Job
                { jobProjectPath = fst $ runningPluginOrigin runningPlugin
                , jobArguments   = readTabSeparatedList line
                }
          hPutStrLn stderr $ "Job added in the queue: " ++ show job
          addToQueue queueTVar job
        return runningPlugin

    threadDelay $ 1000 * 1000
    consumePlugins queueTVar runningPlugins'

  where
    readTabSeparatedList :: String -> [String]
    readTabSeparatedList = unfoldr $ \str ->
      let (part, tabbedRest) = break (== '\t') str
          arg = case reads part of
                  (arg',""):_ -> arg'
                  _           -> part
      in case (str, tabbedRest) of
        ("",_)     -> Nothing
        (_,"")     -> Just (arg, "")
        (_,_:rest) -> Just (arg, rest)
