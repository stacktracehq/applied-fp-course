{ nixpkgs ? import <nixpkgs> {}
, compiler ? "default"
}:
let
  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                      then pkgs.haskellPackages
                      else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage ./applied-fp-course.nix {};
  drvWithTools = pkgs.haskell.lib.addBuildDepends drv [
    pkgs.cabal-install
    pkgs.sqlite
  ];

in
  if pkgs.lib.inNixShell then drvWithTools.env else drv
