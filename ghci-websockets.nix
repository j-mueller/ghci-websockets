{ mkDerivation, aeson, base, foreign-store, stdenv, websockets }:
mkDerivation {
  pname = "ghci-websockets";
  version = "0.0.1";
  src = ./.;
  libraryHaskellDepends = [ aeson base foreign-store websockets ];
  homepage = "https://github.com/j-mueller/ghci-websockets";
  description = "Run a websocket server in GHCi";
  license = stdenv.lib.licenses.bsd3;
}
