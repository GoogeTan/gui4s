with import <nixpkgs> {};
mkShell rec {
  buildInputs = [
    stdenv.cc.cc
    glfw
    openssl
    libz
    libGL
    # ...
  ];
  LD_LIBRARY_PATH = lib.makeLibraryPath buildInputs;
}
