let
  pkgs = import
    (builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/21.05.tar.gz";
    })
    { };

  # To update to a newer version of easy-purescript-nix, run:
  # nix-prefetch-git https://github.com/justinwoo/easy-purescript-nix
  #
  # Then, copy the resulting rev and sha256 here.
  pursPkgs = import
    (pkgs.fetchFromGitHub {
      owner = "justinwoo";
      repo = "easy-purescript-nix";
      rev = "d9a37c75ed361372e1545f6efbc08d819b3c28c8";
      sha256 = "1fklhnddy5pzzbxfyrlprsq1p8b6y9v0awv1a1z0vkwqsd8y68yp";
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
    pursPkgs.purs-tidy
    pkgs.dhall-lsp-server
    pkgs.dhall
    pkgs.nodejs-16_x
    pkgs.nodePackages_latest.purty
    pkgs.nixpkgs-fmt
  ];
}
