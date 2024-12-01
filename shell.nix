{
  pkgs ? import <nixpkgs> { },
}:
pkgs.mkShell {
  nativeBuildInputs = with pkgs; [
    stack
    haskellPackages.haskell-language-server
  ];
}
