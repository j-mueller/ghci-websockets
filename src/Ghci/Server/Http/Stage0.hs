{-# LANGUAGE OverloadedStrings #-}
module Ghci.Server.Http.Stage0 (mkEmbedded) where

import qualified Data.ByteString.Lazy          as BL
import           WaiAppStatic.Storage.Embedded

mkEmbedded :: IO [EmbeddableEntry]
mkEmbedded = do
    file <- BL.readFile "html/index.html"
    let emb = EmbeddableEntry {
                  eLocation = "index.html"
                , eMimeType = "text/html"
                , eContent  = Left (mempty, file)
                }

    return [emb]
