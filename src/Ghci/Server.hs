module Ghci.Server(
  -- $docs
  -- * Starting the server
    start
  , startConfig
  -- * Sending messages
  , sendText
  , sendHtml
  , sendPlot
  -- * Configuration
  , Config
  , Verbosity(..)
  , defaultConfig
  , cfHTTPPort
  , cfVerbosity
  , cfWSPort
  ) where

import           Ghci.Server.Config              (Config, Verbosity (..),
                                                  cfHTTPPort, cfVerbosity,
                                                  cfWSPort, defaultConfig)
import qualified Ghci.Server.Http.Internal       as HTTP
import qualified Ghci.Server.Websockets.Internal as WS
import           Ghci.Server.Websockets.Message  (sendHtml, sendPlot,
                                                  sendText)

-- $docs
-- This modules implements a websocket server whose state survives GHCi
-- reloads. To use it, run 'start' once  per GHCi session, and then
-- use 'sendText', 'sendHtml' and 'sendPlot' to show the
-- values on all clients that are currently connected.

-- | Start the websocket and HTTP servers using the config
startConfig :: Config -> IO ()
startConfig = (<>) <$> HTTP.startConfig <*> WS.startConfig

-- | Start the server with default settings (HTTP on port 3000, websockets
--   on port 9160)
start :: IO ()
start = startConfig defaultConfig
