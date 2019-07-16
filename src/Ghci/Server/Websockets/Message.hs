{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Ghci.Server.Websockets.Message (
  -- $docs
    broadcastText
  , broadcastHtml
  , broadcastPlot
  , Message(..)
  ) where

import           Data.Aeson
import           Data.Text                       (Text)
import qualified Data.Text                       as Text

import           Ghci.Server.Websockets.Internal (broadcast)

-- $docs
-- This module provides the 'Message' data type, and various constructors for
-- it. It is intended to be used together with the 'html/index.html' file.
--
-- = Usage
--
-- 1. Start a GHCi session and run 'Ghci.Websockets.initialiseDef'
-- 2. Open @html/index.html@ in a browser (it's self-contained, no http server
--    required)
-- 3. Use 'broadcastText', 'broadcastHtml' and 'broadcastPlot' to show things
--    in the browser window.

data Message =
  MsgText Text.Text -- ^ A string
  | MsgHtml Text.Text -- ^ An HTML fragment
  | MsgPlotly [Value] Value
  -- ^ The 'data' and 'layout' parameters used for 'Plotly.newPlot'
  --   (see
  --   https://plot.ly/javascript/plotlyjs-function-reference/#plotlynewplot).
  --   The first parameter is a list of Plotly traces (See
  --   https://plot.ly/javascript/reference/,
  --   "Trace types"), the second parameter is a Plotly layout value (see
  --   https://plot.ly/javascript/reference/, "Layout")

instance ToJSON Message where
  toJSON = \case
    MsgText t -> object ["tag" .= ("text" :: String), "contents" .= t]
    MsgHtml t -> object ["tag" .= ("html" :: String), "contents" .= t]
    MsgPlotly dt ly -> object ["tag" .= ("plot" :: String), "contents" .= object ["data" .= dt, "layout" .= ly]]

-- | Show a string.
--
-- >>> broadcastText "hello"
--
broadcastText :: Text -> IO ()
broadcastText = broadcast . MsgText

-- | Insert some HTML into the DOM.
--
-- >>> broadcastHtml "<h1>Hello</h1>"
--
broadcastHtml :: Text -> IO ()
broadcastHtml = broadcast . MsgHtml

-- | Show a Plotly 2D line plot of the given points.
--
-- >>> broadcastPlot [(1, 2), (2, 5), (3, 4), (4, 3)]
--
-- >>> broadcastPlot $ fmap (\i -> let i' = (fromIntegral i / 10) in (i', sin i')) [1..100]
--
broadcastPlot :: [(Double, Double)] -> IO ()
broadcastPlot ns = broadcast (MsgPlotly [dt] ly) where
  ly = object ["margin" .= object ["t" .= (0 :: Int)]]
  dt =
    let (xs, ys) = unzip ns in
    object [ "x" .= xs, "y" .= ys ]
