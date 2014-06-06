{-# LANGUAGE OverloadedStrings #-}

module Court.Result
  ( Results(..)
  , Result(..)
  , modifyGlobalResults
  , modifyLocalResults
  ) where

import           Control.Applicative

import           Data.Aeson hiding (Result)
import qualified Data.ByteString.Lazy.Char8 as BSL

import           System.Directory
import           System.Exit
import           System.FilePath

import           Court.Job
import           Court.Options

newtype Results = Results [Result] deriving (Show, Eq)

instance FromJSON Results where
  parseJSON (Object v) = Results <$> v .: "results"
  parseJSON _ = fail "object expected"

instance ToJSON Results where
  toJSON (Results results) = object [ "results" .= results ]

data Result = Result
  { resultExitCode :: ExitCode
  , resultOutput   :: String
  , resultPath     :: FilePath
  } deriving (Show, Eq)

instance FromJSON Result where
  parseJSON (Object v) =  Result
                      <$> (fmap read $ v .: "exit_code")
                      <*> v .: "output"
                      <*> v .: "path"
  parseJSON _ = fail "object expected"

instance ToJSON Result where
  toJSON result = object
    [ "exit_code"    .= show (resultExitCode result)
    , "output"       .= resultOutput result
    , "path"         .= resultPath result
    , "project_path" .= (snd . splitFileName .takeDirectory . resultPath $ result)
    , "succeed"      .= (resultExitCode result == ExitSuccess)
    ]

modifyGlobalResults :: Options -> (Results -> Results) -> IO ()
modifyGlobalResults opts f = do
  let path = optDirectory opts </> "results.json"
  modifyResults' path f

modifyLocalResults :: Job -> (Results -> Results) -> IO ()
modifyLocalResults job f = do
  let path = jobProjectPath job </> "results.json"
  modifyResults' path f

modifyResults' :: FilePath -> (Results -> Results) -> IO ()
modifyResults' path f = do
  check   <- doesFileExist path
  results <- if check
    then do
      contents <- BSL.readFile path
      return $ either (const (Results [])) id $ eitherDecode contents
    else
      return $  Results []
  BSL.writeFile path $! encode $ f results
