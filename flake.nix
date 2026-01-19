{
  description = "Gui4s development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ] (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          config = {
            allowUnfree = true;
            android_sdk.accept_license = true;
          };
        };

        androidComposition = pkgs.androidenv.composeAndroidPackages {
          buildToolsVersions = [ "35.0.0" ];
          platformVersions = [ "33" "35" ];
          abiVersions = [ "arm64-v8a" "x86_64" ];
          includeEmulator = true;
          includeSystemImages = true;
          systemImageTypes = [ "google_apis_playstore" ];
        };

        androidSdk = androidComposition.androidsdk;

      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = [
            pkgs.mill
            pkgs.jdk21
            androidSdk
            pkgs.glfw
          ];

          ANDROID_HOME = "${androidSdk}/libexec/android-sdk";
          LD_LIBRARY_PATH = "${pkgs.lib.makeLibraryPath [ pkgs.glfw ]}";
        };
      }
    );
}
