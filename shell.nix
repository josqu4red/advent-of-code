{ callPackage, cabal2nix, cabal-install, haskell-language-server, zlib }:
let
  mainPkg = callPackage ./default.nix { };
in mainPkg.overrideAttrs (oa: {
  nativeBuildInputs = [
    cabal2nix
    cabal-install
    haskell-language-server
    zlib.dev
  ] ++ (oa.nativeBuildInputs or [ ]);
})
