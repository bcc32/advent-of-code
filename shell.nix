with import (fetchTarball {
  url =
    "https://github.com/NixOS/nixpkgs/archive/6b86759692b80e2b563e7f6c608f753de4aad3a7.tar.gz";
  sha256 = "12hnn3a568s30519bnkb6bjg7jcwiszhyjnkzw2l8zjnlwzkvv7w";
}) { };
let pkg = ocamlPackages.callPackage ./. { };
in mkShell {
  inputsFrom = [ pkg ];
  buildInputs = pkg.checkInputs ++ [
    ocamlPackages.dune-release
    inotify-tools
    ocamlPackages.merlin
    ocamlformat
    ocamlPackages.ocp-indent
    ocamlPackages.utop
    ruby
    rubyPackages.minitest
  ];
}
