{
  pkgs ? import <nixpkgs> { },
}:
pkgs.mkShell {
  nativeBuildInputs = with pkgs; [
    stack
    (haskell-language-server.override {
      supportedGhcVersions = [
        # "966"
        # "910"
        "9101"
      ];
    })
  ];
}
