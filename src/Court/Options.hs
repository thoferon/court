module Court.Options
  ( Options(..)
  , getOptions
  ) where

import Options.Applicative

data Options = Options
  { optDirectory     :: FilePath
  , optThreads       :: Int
  , optPluginMapPath :: FilePath
  , optUser          :: Maybe String
  , optGroup         :: Maybe String
  , optDaemonize     :: Bool
  } deriving (Show, Eq)

parseOptions :: Parser Options
parseOptions = Options
    <$> strOption (short 'd' <> long "directory"
                             <> help "Directory in which the projects are"
                             <> value ".")
    <*> option (short 't' <> long "num-threads"
                          <> help "Number of threads spawned, therefore, the number of concurrent builds"
                          <> value 2)
    <*> strOption (short 'p' <> long "plugin-map"
                             <> help "Path to the plugin map file"
                             <> value "plugin-map")
    <*> maybeOption (short 'u' <> long "user"
                               <> help "User to run the program with")
    <*> maybeOption (short 'g' <> long "group"
                               <> help "Group to run the program with")
    <*> flag True False (short 'f' <> long "dont-daemonize"
                                   <> help "Do not fork and detach the process from the process group")
  where
    maybeOption :: Mod OptionFields String -> Parser (Maybe String)
    maybeOption mods =
      (\s -> if null s then Nothing else Just s) <$> strOption (mods <> value "")

getOptions :: IO Options
getOptions = do
  execParser $ info parseOptions
    (header "court - Simple and flexible CI tool"
       <> fullDesc)
