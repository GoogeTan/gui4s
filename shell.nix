with import <nixpkgs> {};
mkShell rec {
  buildInputs = [
    sbt-with-scala-native
    scala-next
    metals
    
    stdenv.cc.cc
    glfw
    openssl
    libz
    libGL
  ];
  LD_LIBRARY_PATH = lib.makeLibraryPath buildInputs;
}
