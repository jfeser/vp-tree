{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.05";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, flake-utils, nixpkgs }@inputs:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ self.overlay.${system} ];
        };
      in {
        overlay = self: super: {
          ocamlPackages = super.ocamlPackages.overrideScope' (self: super: {
            vpt = super.buildDunePackage {
              pname = "vpt";
              version = "5.0.0";
              duneVersion = "3";
              src = ./.;
              nativeBuildInputs = [ self.base self.iter self.ppx_jane ];
            };
          });
        };
        defaultPackage = pkgs.ocamlPackages.vpt;
        devShell = pkgs.mkShell {
          nativeBuildInputs =
            [ pkgs.ocamlformat pkgs.opam pkgs.ocamlPackages.ocaml-lsp ];
          inputsFrom = [ pkgs.ocamlPackages.vpt ];
        };
      });
}
