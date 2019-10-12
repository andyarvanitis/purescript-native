#
# You may need to set LC_ALL when building, e.g.
# LC_ALL=en_US.iso88591 stack --nix build
#
{ pkgs ? import <nixpkgs> {}, ghc ? pkgs.ghc }:

pkgs.haskell.lib.buildStackProject {
  name = "default-stack-shell";
  inherit ghc;
  buildInputs = with pkgs; [git git-lfs gmp ncurses zlib];
  LANG = "en_US.iso88591";
}
