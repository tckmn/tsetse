{ pkgs ? import <nixpkgs> {} }:
with pkgs; mkShell {
  nativeBuildInputs = [
    (haskellPackages.ghcWithHoogle (p: with p; [
      random websockets aeson lens
    ]))
    lighttpd
  ];
}
