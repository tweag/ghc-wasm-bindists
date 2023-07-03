{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = inputs: inputs.flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = inputs.nixpkgs.legacyPackages.${system};
      inherit (pkgs) lib haskell;
      hsPkgs = pkgs.haskellPackages.extend (hfinal: hprev: {
        ghc-wasm-bindists = hfinal.developPackage {
          root = lib.sourceFilesBySuffices ./. [ ".cabal" ".hs" ];
          modifier = haskell.lib.compose.overrideCabal {
            enableExecutableProfiling = false;
            doHaddock = false;
          };
        };
      });
    in
    {
      devShells.default = hsPkgs.shellFor {
        packages = p: [ p.ghc-wasm-bindists ];
        nativeBuildInputs = [
          pkgs.cabal-install
        ];
      };
      packages.default = hsPkgs.ghc-wasm-bindists;
    });
}
