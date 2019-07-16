{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
-- | A simple HTTP server that serves the index.html page
module Ghci.Server.Http.Internal where

import           Control.Concurrent             (forkIO)
import           Control.Monad                  (void)
import           Ghci.Server.Http.Stage0        (mkEmbedded)
import           Network.Wai                    (Application)
import           Network.Wai.Application.Static (ssIndices, ssRedirectToIndex,
                                                 staticApp)
import           Network.Wai.Handler.Warp       (run)
import           WaiAppStatic.Storage.Embedded
import           WaiAppStatic.Types             (unsafeToPiece)

import           Ghci.Server.Config             (Config, cfHTTPPort)

theApp :: Application
theApp =
  let st = $(mkSettings mkEmbedded) in
  staticApp $ st { ssRedirectToIndex = True, ssIndices = [unsafeToPiece "index.html"] }

startConfig :: Config -> IO ()
startConfig cf = do
  let p = cfHTTPPort cf
  putStrLn $ "Starting HTTP server on port " ++ show p
  void $ forkIO $ run (cfHTTPPort cf) theApp
