{ lib, nix-gitignore, buildDunePackage, angstrom, async, bignum, cmdliner, core
, core_bench, delimited_parsing, expect_test_helpers_async, iter, re, re2
, topological_sort, yojson }:

buildDunePackage rec {
  pname = "aoc2015";
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
    iter
    re
    re2
    # TODO: Standardize on one regular expression library
    topological_sort
    yojson
  ];
  meta = { homepage = "https://github.com/bcc32/advent-of-code-2015"; };
  passthru.checkInputs = checkInputs;
}
