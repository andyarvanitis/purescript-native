{
  description = "The PureScript Native Go Compiler Backend";

  inputs = {
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    flake-utils.url = "github:numtide/flake-utils";
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  };

  outputs = { self, flake-compat, flake-utils, haskellNix, nixpkgs }:
    let
      systems = with flake-utils.lib.system; [
        x86_64-darwin
        x86_64-linux
      ];

      overlay = final: prev: {
        # This dummy package is required in building the project, but `hsc2hs` is actually provided by GHC.
        hsc2hs = final.stdenv.mkDerivation {
          name = "hsc2hs";
          unpackPhase = "true";
          installPhase = "mkdir -p $out";
        };

        purescript-native-go = final.haskell-nix.project' {
          src = ./.;
        };
      };

      perSystem = system:
        let
          # We use both vanilla/upstream nixpkgs (as pinned by haskell.nix), and the patched/overlayed version.
          # This is so that we can pull more things from binary cache.

          pkgs = import nixpkgs {
            inherit system;
          };

          pkgs' = import nixpkgs {
            inherit system;
            inherit (haskellNix) config;
            overlays = [ haskellNix.overlay overlay ];
          };

          # A few utility functions for adding short/convenience app and package names to the flake
          # The only reason these are per-system is because they leverage nixpkgs' lib

          shortStuff = idx: attrs:
            let
              mapped = pkgs.lib.mapAttrs' (k: v:
                let
                  parts = builtins.split ":exe:" k;
                  len = builtins.length parts;
                  pair = if len == 3
                    then pkgs.lib.nameValuePair (builtins.elemAt parts idx) v
                    else pkgs.lib.nameValuePair k null;
                in pair
              ) attrs;
              filtered = pkgs.lib.filterAttrs (k: v: v != null) mapped;
            in filtered;

          shortApps = final: prev: shortStuff 2 prev;
          shortPackages = final: prev: shortStuff 0 prev;

          # Where the fun stuff begins...

          flake = pkgs'.purescript-native-go.flake {
            packages =
              let included = [ "psgo" "purescript" ];
              in ps: pkgs.lib.filterAttrs (k: v: builtins.elem k included) ps;
          };

          extendFlake = final: prev:
            let
              # `hlint` is pulled in from regular nixpkgs because building with haskell.nix is failing for me due to version conflicts with `ghc-lib-parser`.
              # `spago` won't build on my machine (due to `system-fileio` test failures likely related to my use of ZFS), so I need to grab it from a binary cache.
              # Other CLI tools were brought in from pkgs (vs pkgs', `tools {}`, or packages/components in the flake) for caching and/or simplicity.
              # Using the derivations inside the flake would pin CLI tools to the currently specified Stack resolver.

              hackingShell = pkgs'.purescript-native-go.shellFor {
                buildInputs = with pkgs; [
                  bashInteractive
                  cabal-install
                  haskell-language-server
                  hlint
                  nixpkgs-fmt
                  stack
                ];
                exactDeps = true;
                withHoogle = true;
              };

              usingShell = pkgs'.purescript-native-go.shellFor {
                buildInputs = (with pkgs; [
                  bashInteractive
                  delve
                  go
                  go-tools
                  gopkgs
                  spago
                ]) ++ (with final.packages; [
                  psgo
                  purescript
                ]);
                exactDeps = true;
                withHoogle = false;
              };

            in {
              apps = pkgs.lib.fix (pkgs.lib.extends shortApps (pkgs.lib.const prev.apps));
              defaultApp = final.apps.psgo;

              packages = pkgs.lib.fix (pkgs.lib.extends shortPackages (pkgs.lib.const prev.packages));
              defaultPackage = final.packages.psgo;

              devShell = final.devShells.hacking-on-psgo;
              devShells = {
                hacking-on-psgo = hackingShell;
                using-psgo = usingShell;
              };
            };
        in
          pkgs.lib.fix (pkgs.lib.extends extendFlake (pkgs.lib.const flake));

    in { inherit overlay; } // flake-utils.lib.eachSystem systems perSystem;
}
