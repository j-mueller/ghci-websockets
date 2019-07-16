module Ghci.Server.Config(
  Config
  , defaultConfig
  , cfWSPort
  , cfHTTPPort
  , cfVerbosity
  , Verbosity(..)
  , logStr
   ) where

-- | What to do with log messages
data Verbosity =
  Verbose -- ^ Write all log messages to stdout
  | Silent -- ^ Ignore all log messages
  deriving (Eq, Ord, Show)

-- | Server configuration
data Config =
  Config
  { cfWSPort    :: Int -- ^ Websocket port
  , cfHTTPPort  :: Int -- ^ HTTP port
  , cfVerbosity :: Verbosity -- ^ What to do with log messages
  }

-- | Log a message according to the configured 'Verbosity'
logStr :: Config -> String -> IO ()
logStr c s = case cfVerbosity c of
  Silent  -> pure ()
  Verbose -> putStrLn s

-- | Default config, use ports 9160 (websockets) and 3000 (http) and ignore all
--   log messages.
defaultConfig :: Config
defaultConfig = Config 9160 3000 Silent
