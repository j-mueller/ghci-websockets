# ghci-websockets

With `ghci-websockets` you can send data from GHCi straight to the browser, using a websocket connection that survives GHCi reloads. 

![ghci-websockets.gif](ghci-websockets.gif)

## Contents

The `Ghci.Websockets` module implements the actual websocket server, broadcasting JSON objects to all clients. `Ghci.Websockets.Simple` adds a custom message type for text, HTML, and plots on top of that.

## Quickstart

* Add `ghci-websockets` to the `build-depends` field of your .cabal file
* Run `cabal new-repl`
* Run `Ghci.Websockets.Simple.initialiseDef`
* Open `html/index.html` in a browser
* In GHCi, run `Ghci.Websockets.Simple.broadcastText "hello"` (see also `broadcastHtml` and `broadcastPlot` from the same module). You may need `:set -XOverloadedStrings`.

## Warning

This packages uses the `foreign-store` package internally, which is highly unstable. I wouldn't use `ghci-websockets` for anything other than GHCi.

## License

BSD-3-Clause, see LICENSE

## Contributions

Bug reports, pull requests etc. are welcome!
