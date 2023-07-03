{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, flake-utils, nixpkgs }@inputs:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        ocamlPkgs = pkgs.ocaml-ng.ocamlPackages;
        vpt = ocamlPkgs.buildDunePackage {
          pname = "vpt";
          version = "5.0.0";
          duneVersion = "3";
          src = ./.;
          propagatedBuildInputs = with ocamlPkgs; [ base iter ];
        };
      in {
        defaultPackage = vpt;
        devShell = pkgs.mkShell {
          nativeBuildInputs = [
            pkgs.ocamlformat
            pkgs.opam
            pkgs.ocamlPackages.ocaml-lsp
            pkgs.ocamlPackages.ppx_jane
          ];
          inputsFrom = [ vpt ];
        };
      });
}
