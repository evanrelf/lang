let
  pkgs = import <nixpkgs> { };

  lang = pkgs.haskellPackages.callCabal2nix "lang" ./. { };

in
lang.env.overrideAttrs (prev: {
  buildInputs = [
    pkgs.cabal-install
    pkgs.ghcid
  ];
})
