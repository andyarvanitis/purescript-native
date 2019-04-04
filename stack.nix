{ pkgs ? import <nixpkgs> {}, ghc ? pkgs.ghc }:

pkgs.haskell.lib.buildStackProject {
  name = "default-stack-shell";
  inherit ghc;
  buildInputs = with pkgs; [git git-lfs gmp zlib];
  LANG = "en_US.UTF-8";
}
