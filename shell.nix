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
      rev = "aa72388ca0fb72ed64467f59a121db1f104897db";
      sha256 = "1j37v3ncnakhq7p4l2vqdn4li8bgwcc8cd2hk2fblxhnlglikgx2";
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
    pkgs.nodejs
    pkgs.nixpkgs-fmt
  ];
}
