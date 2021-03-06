let
  pkgs = import
    (builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/20.09.tar.gz";
    })
    { };

  # To update to a newer version of easy-purescript-nix, run:
  # nix-prefetch-git https://github.com/justinwoo/easy-purescript-nix
  #
  # Then, copy the resulting rev and sha256 here.
  # Last update: 2020-08-01
  pursPkgs = import
    (pkgs.fetchFromGitHub {
      owner = "justinwoo";
      repo = "easy-purescript-nix";
      rev = "734ca9c00038f4b549bf8fc58eb65e08a87e9d56";
      sha256 = "1cn2gmd55bcx97xi7v59m4xw4y86v60p85x460jbb8bn6cfy6xmc";
    })
    { inherit pkgs; };

in
pkgs.stdenv.mkDerivation {
  name = "purescript-webpack-template";
  buildInputs = with pursPkgs; [
    pkgs.nix-prefetch-git
    pursPkgs.purs
    pursPkgs.spago
    pursPkgs.zephyr
    pkgs.dhall-lsp-server
    pkgs.dhall
    pkgs.nodejs-14_x
    pkgs.nodePackages_latest.purty
    pkgs.nixpkgs-fmt
  ];
}
