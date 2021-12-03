{ lib, nix-gitignore, buildDunePackage, angstrom, async, bignum, core, expect_test_helpers_async, re, topological_sort }:

buildDunePackage rec {
  pname = "aoc2015";
  version = "0.1.0";
  useDune2 = true;
  src = nix-gitignore.gitignoreFilterSource lib.cleanSourceFilter [ ] ./.;
  checkInputs = [];
  buildInputs = [ angstrom async bignum core expect_test_helpers_async re topological_sort ];
  meta = { homepage = "https://github.com/bcc32/advent-of-code-2015"; };
  passthru.checkInputs = checkInputs;
}
