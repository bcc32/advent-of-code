with import <nixpkgs> { };

let
  inherit (ocamlPackages)
    buildDunePackage async bignum core expect_test_helpers re topological_sort;

in buildDunePackage {
  pname = "aoc2020";
  version = "0.1.0";
  useDune2 = true;
  src = nix-gitignore.gitignoreFilterSource lib.cleanSourceFilter [ ] ./.;
  buildInputs = [ async bignum core expect_test_helpers re topological_sort ];
  meta = { homepage = "https://github.com/bcc32/advent-of-code-2020"; };
}
