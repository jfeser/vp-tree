{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, flake-utils, nixpkgs }@inputs:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlay = final: prev: {
          ocamlPackages = prev.ocamlPackages.overrideScope' (ofinal: oprev: {
            vpt = ofinal.buildDunePackage {
              pname = "vpt";
              version = "5.0.0";
              duneVersion = "3";
              src = ./.;
              propagatedBuildInputs = with ofinal; [ base iter ];
            };
          });
        };
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ overlay ];
        };
      in {
        overlays.default = overlay;
        defaultPackage = pkgs.ocamlPackages.vpt;
        devShell = pkgs.mkShell {
          nativeBuildInputs = [
            pkgs.ocamlformat
            pkgs.opam
            pkgs.ocamlPackages.ocaml-lsp
            pkgs.ocamlPackages.ppx_jane
          ];
          inputsFrom = [ self.defaultPackage.${system} ];
        };
      });
}
