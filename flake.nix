{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = import nixpkgs { inherit system; };
      in {
        packages.default = pkgs.ocamlPackages.buildDunePackage {
          pname = "turbo-opam";
          version = "n/a";
          src = ./.;
          propagatedBuildInputs = with pkgs.ocamlPackages; [
            cmdliner
            menhir
            opam-format
            pp_loc
          ];
        };
        devShells.default = pkgs.mkShell {
          inputsFrom = [ self.packages.${system}.default ];
          nativeBuildInputs = with pkgs.ocamlPackages; [
            merlin
            ocamlformat_0_26_1
          ];
        };
      });
}
