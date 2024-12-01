{
  pkgs ? import <nixpkgs> { },
}:
pkgs.mkShell {
  nativeBuildInputs = with pkgs; [
    stack
    haskellPackages.haskdogs
    haskellPackages.hasktags
    haskellPackages.haskell-language-server
    ghcid
  ];
}
