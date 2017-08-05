{ nixpkgs ? import ./nixpkgs {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  overrides =
    if builtins.pathExists ./overrides.nix
    then import ./overrides.nix pkgs pkgs.haskell.lib
    else self: super: {};

  haskellPackages =
    if compiler == "default"
    then pkgs.haskellPackages.override { inherit overrides; }
    else pkgs.haskell.packages.${compiler}.override { inherit overrides; };

  drv = haskellPackages.callPackage ./. {};

in

  if pkgs.lib.inNixShell then drv.env else drv
