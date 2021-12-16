{ lib, nix-gitignore, buildDunePackage, angstrom, async, bignum, cmdliner, core
, core_bench, delimited_parsing, expect_test_helpers_async, re
, topological_sort, yojson }:

buildDunePackage rec {
  pname = "aoc";
  version = "0.1.0";
  useDune2 = true;
  src = nix-gitignore.gitignoreFilterSource lib.cleanSourceFilter [ ] ./.;
  checkInputs = [ ];
  buildInputs = [
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
  passthru.checkInputs = checkInputs;
}
