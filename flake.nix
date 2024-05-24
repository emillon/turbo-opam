{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = import nixpkgs { inherit system; };
      in {
        formatter = pkgs.nixpkgs-fmt;
        packages.default = pkgs.ocamlPackages.buildDunePackage {
          pname = "turbo-opam";
          version = "n/a";
          src = ./.;
          nativeBuildInputs = with pkgs.ocamlPackages; [ menhir ];
          buildInputs = with pkgs.ocamlPackages; [
            cmdliner
            menhir
            opam-format
            pp_loc
          ];
        };
        packages.bench = let
          bin = self.packages.${system}.default + "/bin/turbo-opam";
          repository = pkgs.fetchFromGitHub {
            owner = "ocaml";
            repo = "opam-repository";
            rev = "6ed19e325e5016a43606d8073ca73998f9ebf68f";
            hash = "sha256-BNq/+jmdi9TkUr8WX6NZpr5QXUsKqUvDaxt6Kh8HI6Y=";
          };
        in pkgs.stdenv.mkDerivation {
          name = "benchmark";
          dontUnpack = true;
          buildPhase = ''
            ${pkgs.hyperfine}/bin/hyperfine \
              '${bin} bench ${repository} --parser control' \
              '${bin} bench ${repository} --parser experiment' \
              > bench.txt
          '';
          installPhase = ''
            mkdir $out
            mv bench.txt $out/
          '';
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
