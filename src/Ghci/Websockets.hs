{-# LANGUAGE OverloadedStrings #-}
module Ghci.Websockets(
  -- $docs
  --
    initialise
  , initialiseDef
  , broadcast
  -- * Configuration
  , Config(..)
  , Verbosity(..)
  , defaultConfig
  ) where

import           Control.Concurrent (MVar, forkIO, modifyMVar, modifyMVar_,
                                     newMVar, readMVar)
import           Control.Exception  (catch)
import           Control.Monad      (void, (>=>))
import           Data.Aeson         (ToJSON)
import qualified Data.Aeson         as Aeson
import           Data.Foldable      (traverse_)
import qualified Data.Map           as Map
import qualified Data.Text          as Text
import qualified Foreign.Store      as Store
import qualified Network.WebSockets as WS

-- $docs
-- This modules implements a websocket server whose state survives GHCi 
-- reloads. To use it, run 'initialiseDef' once  per GHCi session, and then 
-- call 'broadcast' to send a JSON value to all clients that are currently 
-- connected. All messages from clients are ignored.

-- | What to do with log messages
data Verbosity =
      Verbose -- ^ Write all log messages to stdout
    | Silent -- ^ Ignore all log messages
    deriving (Eq, Ord, Show)

-- | Server configuration
data Config =
  Config
    { port      :: Int -- ^ What port to start the server on
    , verbosity :: Verbosity -- ^ What to do with log messages
    }

logStr :: Config -> String -> IO ()
logStr c s = case verbosity c of
  Silent  -> pure ()
  Verbose -> putStrLn s

-- | Default config, use port 9160 and ignore all log messages.
defaultConfig :: Config
defaultConfig = Config 9160 Silent

newtype ServerState = ServerState { unServerState :: Map.Map ConnectionID WS.Connection }

type ConnectionID = Int

serverState :: ServerState
serverState = ServerState Map.empty

addConnection :: WS.Connection -> ServerState -> (ServerState, ConnectionID)
addConnection c (ServerState mp) = (ServerState (Map.insert k c mp), k) where
  k = maybe 0 (succ . fst) $ Map.lookupMax mp

deleteConnection :: ConnectionID -> ServerState -> ServerState
deleteConnection i (ServerState mp) = ServerState (Map.delete i mp)

theStore :: Store.Store (MVar ServerState)
theStore = Store.Store 0

-- | Send a JSON object to all clients. Throws an exception if 'initialise' has
--   not been run first.
broadcast :: ToJSON a => a -> IO ()
broadcast t = Store.withStore theStore (readMVar >=> go) where
  go s = traverse_ (`WS.sendTextData` msg) (unServerState s)
  msg = Aeson.encode t

-- | Start the websocket server using the default config (port 9160). Call once
--   per GHCi session.
initialiseDef :: IO ()
initialiseDef = initialise defaultConfig

-- | Start the websocket server using the port specified in the config. Call
--   once per GHCi session.
initialise :: Config -> IO ()
initialise c@Config{port=p} = do
  state <- newMVar serverState
  Store.writeStore theStore state
  logStr c ("Starting websocket server on port " ++ show p)
  void $ forkIO (WS.runServer "127.0.0.1" p (application c))

application :: Config -> WS.ServerApp
application conf pending = do
  state <- Store.readStore theStore
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30
  connID <- modifyMVar state (pure . addConnection conn)
  logStr conf $ "Accepted connection " ++ show connID
  let go = (WS.receiveData conn >>= logStr conf . Text.unpack) >> go
  catch go (closeConnection conf connID)

closeConnection :: Config -> ConnectionID -> WS.ConnectionException -> IO ()
closeConnection conf connID ex = do
  logStr conf $ "Closing connection " ++ show connID ++ " due to " ++ show ex
  state <- Store.readStore theStore
  modifyMVar_ state (pure . deleteConnection connID)
