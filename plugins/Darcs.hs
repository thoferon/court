import Control.Applicative
import Control.Concurrent
import Control.Monad

import Data.List

import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Process

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  args <- getArgs
  repo <- case args of
    [] -> hPutStrLn stderr "Please pass the repository as argument" >> exitFailure
    r : _ -> return r

  testRepoPath <- (</> "test-repo") <$> getCurrentDirectory
  checkExistence <- doesDirectoryExist testRepoPath

  unless checkExistence $ do
    hPutStrLn stderr $ "Getting " ++ repo ++ " ..."
    exitCode <- rawSystem "darcs" ["get", repo, testRepoPath, "--lazy"]
    unless (exitCode == ExitSuccess) $ exitWith exitCode

  setCurrentDirectory testRepoPath

  forever $ do
    hPutStrLn stderr "Checking ..."
    output <- readProcess "darcs" ["pull", "--all"] ""
    unless ("No remote changes to pull in" `isInfixOf` output) $ putStrLn testRepoPath
    threadDelay $ 30 * 1000 * 1000
