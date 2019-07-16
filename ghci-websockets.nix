{ mkDerivation, aeson, base, bytestring, containers, foreign-store
, stdenv, text, wai, wai-app-static, warp, websockets
}:
mkDerivation {
  pname = "ghci-websockets";
  version = "0.0.1";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring containers foreign-store text wai
    wai-app-static warp websockets
  ];
  homepage = "https://github.com/j-mueller/ghci-websockets";
  description = "A websocket server that survives GHCi reloads";
  license = stdenv.lib.licenses.bsd3;
}
