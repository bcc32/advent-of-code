with import <nixpkgs> { };

let
  inherit (ocamlPackages)
    buildDunePackage async bignum core expect_test_helpers_async
    expect_test_helpers_core re topological_sort;

  euler = import ../euler;

in buildDunePackage {
  pname = "aoc2020";
  version = "0.1.0";
  useDune2 = true;
  doCheck = true;
  src = nix-gitignore.gitignoreFilterSource lib.cleanSourceFilter [ ] ./.;
  buildInputs = [
    async
    bignum
    core
    euler
    expect_test_helpers_async
    expect_test_helpers_core
    re
    topological_sort
  ];
  meta = { homepage = "https://github.com/bcc32/advent-of-code-2020"; };
}
