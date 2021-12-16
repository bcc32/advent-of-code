{ pkgs ? import <nixpkgs> { } }:

pkgs.mkShell { buildInputs = [ pkgs.ruby pkgs.rubyPackages.minitest ]; }
