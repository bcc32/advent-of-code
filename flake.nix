{
  description = "Advent of Code solutions";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    ocaml-overlays.url = "github:nix-ocaml/nix-overlays";
    ocaml-overlays.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, flake-utils, nixpkgs, ocaml-overlays }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ ocaml-overlays.overlays.default ];
        };
      in with pkgs; rec {
        devShells.default =
          mkShell { inputsFrom = [ devShells.ocaml devShells.ruby ]; };

        devShells.ocaml = mkShell {
          inputsFrom = [ packages.default ];
          buildInputs = lib.optional stdenv.isLinux inotify-tools ++ [
            ocamlPackages.merlin
            ocamlformat
            ocamlPackages.ocp-indent
            ocamlPackages.utop
          ];
        };

        devShells.ruby = let
          gems = bundlerEnv {
            name = "env";
            gemdir = ./.;
          };
        in mkShell { buildInputs = [ gems gems.wrappedRuby bundix ]; };

        packages.default = ocamlPackages.buildDunePackage rec {
          pname = "aoc";
          version = "0.1.0";
          useDune2 = true;
          src = ./.;
          buildInputs = with ocamlPackages; [
            angstrom
            async
            bignum
            cmdliner
            core
            core_bench
            delimited_parsing
            expect_test_helpers_async
            re
            topological_sort
            yojson
          ];
          meta = { homepage = "https://github.com/bcc32/advent-of-code"; };
        };
      });
}
