let
  pkgs = import ./nixpkgs-from-json.nix { json = ./2019-04-24-nixpkgs.json; };

  dontCheck = pkgs.haskell.lib.dontCheck;
  dontHaddock = pkgs.haskell.lib.dontHaddock;

  # https://github.com/NixOS/nixpkgs/issues/26561#issuecomment-354862897
  haskellPackages = pkgs.haskellPackages.override (old: {
    overrides = pkgs.lib.composeExtensions (old.overrides or (_: _: {})) (self: super: {

      parseargs = dontCheck (super.parseargs);

      ghci-websockets = self.callPackage ./ghci-websockets.nix { };


      });

  });
in { 
  haskellPackages = haskellPackages;
  pkgs = pkgs;
}
