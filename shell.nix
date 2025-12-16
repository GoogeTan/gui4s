with import <nixpkgs> {};
mkShell rec {
  buildInputs = [
    glfw
    mill
  ];
  LD_LIBRARY_PATH = lib.makeLibraryPath buildInputs;
}
