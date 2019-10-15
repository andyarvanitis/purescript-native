#
# You may need to manually run `unset STACK_IN_NIX_SHELL`
# (doesn't work from a `shellHook`; see: https://github.com/commercialhaskell/stack/issues/5000
#
#
# You may need to set LC_ALL when building, e.g.
# LC_ALL=en_US.iso88591 stack --nix build; see e.g. https://github.com/koalaman/shellcheck/issues/324
# 
#
{ pkgs ? import <nixpkgs> {}, ghc ? pkgs.ghc }:

pkgs.haskell.lib.buildStackProject {
  name = "default-stack-shell";
  inherit ghc;
  buildInputs = with pkgs; [git git-lfs gmp ncurses zlib];
  LANG = "en_US.UTF-8";
}
