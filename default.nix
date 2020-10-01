with import <nixpkgs> { };

let
  inherit (ocamlPackages)
    buildDunePackage async async_ssl cmdliner core cohttp cohttp-async yojson;

in buildDunePackage {
  pname = "danbooru-tool";
  version = "0.4.1";
  useDune2 = true;
  src = nix-gitignore.gitignoreFilterSource lib.cleanSourceFilter [ ] ./.;
  buildInputs = [ async async_ssl cmdliner core cohttp cohttp-async yojson ];
  meta = { homepage = "https://github.com/bcc32/danbooru-tool"; };
}
