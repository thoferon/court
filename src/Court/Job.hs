module Court.Job
  ( Job(..)
  ) where

data Job = Job
  { jobProjectPath :: FilePath
  , jobArguments   :: [String]
  } deriving (Show, Eq)
