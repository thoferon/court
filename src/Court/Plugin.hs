module Court.Plugin
  ( Plugin(..)
  , RunningPlugin(..)
  , PluginMap(..)
  , pluginMapToPairs
  ) where

import System.FilePath
import System.IO
import System.Process

data Plugin = Plugin
  { pluginExecutable :: FilePath
  , pluginArguments  :: [String]
  } deriving (Show, Read, Eq)

data RunningPlugin = RunningPlugin
  { runningPluginStdout  :: Handle
  , runningPluginProcess :: ProcessHandle
  , runningPluginOrigin  :: (FilePath, Plugin)
  }

newtype PluginMap = PluginMap [(FilePath, [Plugin])] deriving (Show, Read, Eq)

pluginMapToPairs :: FilePath -> PluginMap -> [(FilePath, Plugin)]
pluginMapToPairs basePath (PluginMap pairs) =
  let mkPair path plugin = (if isRelative path then basePath </> path else path, plugin)
  in concat $ map (\(path, plugins) -> map (\plugin -> mkPair path plugin) plugins) pairs
