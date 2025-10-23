with import <nixpkgs> {};
mkShell rec {
  buildInputs = [
    glfw
  ];
  LD_LIBRARY_PATH = lib.makeLibraryPath buildInputs;
}
