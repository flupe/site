let
  compiler = "ghc884";
  rev  = "b78e08e981a9ad31036fc6c6fb880c1315b4ebea";
  overlay = _: pkgs: {
    haskellPackages = pkgs.haskellPackages.override {
      overrides = self: super: rec {
        achille = self.callPackage ../achille/achille.nix {};
      };
    };
  };
  nixpkgs =
    import (builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    }) { overlays = [ overlay ] ; };

in nixpkgs.haskellPackages.callPackage ./acatalepsie.nix {}
