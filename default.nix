let
  pkgs = import ./nixpkgs-from-json.nix { json = ./2019-04-24-nixpkgs.json; };

  dontCheck = pkgs.haskell.lib.dontCheck;
  dontHaddock = pkgs.haskell.lib.dontHaddock;

  ghcjsPackages = pkgs.haskell.packages.ghcjs86.override (old: {
    overrides = pkgs.lib.composeExtensions (old.overrides or (_: _: {})) (self: super: {

      core = self.callPackage ./nix/core-ghcjs.nix { };
      
      # packages with broken test suites (they need dontCheck)
      aeson = dontCheck (self.callPackage ./nix/aeson-ghcjs.nix { });
      algebraic-graphs = dontCheck (super.algebraic-graphs);
      bound = dontCheck (super.bound);
      bytes = dontCheck (super.bytes);
      comonad = dontCheck (super.comonad);
      ghcjs-fetch = dontCheck (self.callPackage ./nix/ghcjs-fetch-ghcjs.nix { });
      format-numbers = dontCheck (super.format-numbers);
      http-types = dontCheck (super.http-types);
      lens = dontCheck (super.lens);
      linear = dontCheck (super.linear);
      megaparsec = dontCheck (super.megaparsec);
      prettyprinter = dontCheck (super.prettyprinter);
      QuickCheck = dontCheck (super.QuickCheck);
      semigroupoids = dontCheck (super.semigroupoids);
      scientific = dontCheck (super.scientific);
      tasty-quickcheck = dontCheck (super.tasty-quickcheck);
      transformers-base = dontCheck (super.transformers-base);

      # concur stuff
      concur-core = self.callPackage ./nix/concur-core-ghcjs.nix { };
      concur-react = self.callPackage ./nix/concur-react-ghcjs.nix { };

      # h3
      h3-core = self.callPackage ./nix/h3-core-ghcjs.nix { };
      h3-colour = self.callPackage ./nix/h3-colour-ghcjs.nix { };
      h3-concur-react = self.callPackage ./nix/h3-concur-react-ghcjs.nix { };

    });
  });
  
  # https://github.com/NixOS/nixpkgs/issues/26561#issuecomment-354862897
  haskellPackages = pkgs.haskellPackages.override (old: {
    overrides = pkgs.lib.composeExtensions (old.overrides or (_: _: {})) (self: super: {

      ghci-websockets = self.callPackage ./ghci-websockets.nix { };


      });

  });
in { 
  haskellPackages = haskellPackages;
  ghcjsPackages = ghcjsPackages;
  pkgs = pkgs;
}
