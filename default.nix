{ pkgs ? import <nixos> {} }:
with pkgs;
stdenv.mkDerivation {
  name = "clam";
  buildInputs = [
    postgresql_12
  ];
}
