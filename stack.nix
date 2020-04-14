#
# You may need to manually run `unset STACK_IN_NIX_SHELL`
# (doesn't work from a `shellHook`; see: https://github.com/commercialhaskell/stack/issues/5000
#
#
# You may need to set LC_ALL when building, e.g.
# LC_ALL=en_US.iso88591 stack --nix build; see e.g. https://github.com/koalaman/shellcheck/issues/324
# 
#
{ pkgs ?
  import (builtins.fetchTarball {
    name = "nixos-release-19.03";
    url = https://github.com/nixos/nixpkgs/archive/34c7eb7545d155cc5b6f499b23a7cb1c96ab4d59.tar.gz;
    sha256 = "11z6ajj108fy2q5g8y4higlcaqncrbjm3dnv17pvif6avagw4mcb";
  }) {}
}:

pkgs.haskell.lib.buildStackProject {
  name = "default-stack-shell";
  ghc = pkgs.haskell.compiler.ghc865;
  buildInputs = with pkgs; [git git-lfs gmp ncurses zlib];
  LANG = "en_US.UTF-8";
}
