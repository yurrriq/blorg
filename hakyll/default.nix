{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, filepath, hakyll, split, stdenv, text
      }:
      mkDerivation {
        pname = "blorg";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [ base filepath hakyll split text ];
        buildDepends = if stdenv.isDarwin
                          then [ pkgs.darwin.apple_sdk.frameworks.Cocoa ]
                          else [];
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
