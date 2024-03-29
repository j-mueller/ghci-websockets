{
  bootstrap ? import <nixpkgs> {}
, json
}:
let 
  nixpkgs = builtins.fromJSON (builtins.readFile json);
    src = bootstrap.fetchFromGitHub {
    owner = "NixOS";
    repo  = "nixpkgs";
    inherit (nixpkgs) rev sha256;
  };
in 
  import src {} 