{
  description = "Moore Machine Based LLM Agents";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixos-23.11;
    flake-utils.url = github:numtide/flake-utils;
  };

  outputs = { self, nixpkgs, flake-utils }:
    let
      overlay = import ./overlay.nix;
      overlays = [ overlay ];
    in
    flake-utils.lib.eachSystem [ "x86_64-linux" ]
      (system:
        let pkgs = import nixpkgs { inherit system overlays; };
        in rec {
          devShells.default = pkgs.haskellPackages.shellFor {
            packages = p: [ p.agent-moore ];
            buildInputs = [
              pkgs.haskellPackages.cabal-install
              pkgs.haskellPackages.ghc
              pkgs.haskellPackages.haskell-language-server
              pkgs.nixfmt

              pkgs.pkg-config
            ];
          };
          formatter = pkgs.nixpkgs-fmt;
          packages.default = pkgs.haskellPackages.agent-moore;
          packages.co-prompt = pkgs.haskellPackages.agent-moore;
        }) // {
      overlays.default = overlay;
    };
}
