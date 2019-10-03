with import <nixpkgs> {};

mkShell {
  buildInputs = [
    (haskellPackages.callPackage ./release.nix {})
  ];
}
