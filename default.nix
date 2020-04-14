let
  haskellNix = import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz) {};
  nixpkgsSrc = haskellNix.sources.nixpkgs-1903; # has hsc2hs-0.68.4

  hsc2hsOverlay = self: super: {
    buildPackages = super.buildPackages // {
      hsc2hs = super.haskellPackages.hsc2hs;
    };
  };

  nixpkgsArgs = haskellNix.nixpkgsArgs // {
    overlays = haskellNix.nixpkgsArgs.overlays ++ [ hsc2hsOverlay ];
  };
in
{ pkgs ? import nixpkgsSrc nixpkgsArgs
}:

pkgs.haskell-nix.stackProject {
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "purescript-native";
    src = ./.;
  };
}
