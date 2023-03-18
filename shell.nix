{ pkgs ? import <nixpkgs> {} }:

let
  easy-ps = import ./nix/easy-purescript.nix { inherit pkgs; };
in
  pkgs.mkShell {
    buildInputs = [
      easy-ps.purs
      easy-ps.spago
      easy-ps.purs-tidy
      pkgs.nodejs-18_x
    ];

    shellHook = ''
      export PATH=$PWD/node_modules/.bin:$PATH
    '';
  }