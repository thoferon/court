import Control.Concurrent

import System.IO
import System.Posix.Process
import System.Posix.User

import Court.Builder
import Court.Options
import Court.PluginManager
import Court.Queue

main :: IO ()
main = do
  hSetBuffering stderr LineBuffering
  opts <- getOptions

  case optGroup opts of
    Nothing -> return ()
    Just name -> do
      groupEntry <- getGroupEntryForName name
      setGroupID $ groupID groupEntry

  case optUser opts of
    Nothing -> return ()
    Just name -> do
      userEntry <- getUserEntryForName name
      setUserID $ userID userEntry

  if optDaemonize opts
    then forkProcess (createSession >> main' opts) >> return ()
    else main' opts

main' :: Options -> IO ()
main' opts = do
  queueTVar <- newTVarIO mempty
  _ <- forkIO $ builder opts queueTVar
  pluginManager opts queueTVar
