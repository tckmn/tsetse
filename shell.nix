{ pkgs ? import <nixpkgs> {} }:
with pkgs; mkShell {
  nativeBuildInputs = [ (haskellPackages.ghcWithPackages ( p: with p; [ random websockets aeson ] )) ];
}
