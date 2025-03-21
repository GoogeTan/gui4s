with import <nixpkgs> {};
mkShell rec {
  buildInputs = [
    stdenv.cc.cc
    glfw
    openssl
    libz
	vulkan-tools
	vulkan-validation-layers
	vulkan-extension-layer
	vulkan-headers
	vulkan-loader
	vulkan-utility-libraries
    # ...
  ];
  LD_LIBRARY_PATH = lib.makeLibraryPath buildInputs;
}
