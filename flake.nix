{
  description = "Gui4s development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-26.05-darwin";
    flake-utils.url = "github:numtide/flake-utils";
    mill-flake.url = "path:/Users/katze/nix/mill-flake";
  };

  outputs = { self, nixpkgs, flake-utils, mill-flake }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ] (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          config = {
            allowUnfree = true;
            android_sdk.accept_license = true;
          };
        };
        jdk = pkgs.jdk21;
        mill = mill-flake.lib.${system}.mkMillFromFile {
            filePath = ./.mill-version;
            hash = {
                "aarch64-darwin" = "sha256-WvBjrpQIWnA1lswCuzkPjo+QHQQ9O7Vpz2bg2xzRSUM=";
                "x86_64-linux" = "sha256-umDQNXc4CpBsInPMp2VoKHBeJf71GaHpJ2Vq4Wfo9kQ=";
            };
            inherit jdk;
        };
        androidComposition = pkgs.androidenv.composeAndroidPackages {
          cmdLineToolsVersion = "19.0";  #At this point Mill expects 19 version and only it
          buildToolsVersions = [ "35.0.0" ];
          platformVersions = [ "33" "35" ];
          abiVersions = [ "arm64-v8a" "x86_64" ];
          includeEmulator = false;
          includeSystemImages = false;
          systemImageTypes = [ "google_apis_playstore" ];
        };

        androidSdk = androidComposition.androidsdk;

      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = [
            mill
            jdk
            androidSdk
            pkgs.glfw
          ];

          ANDROID_HOME = "${androidSdk}/libexec/android-sdk";
          LD_LIBRARY_PATH = "${pkgs.lib.makeLibraryPath [ pkgs.glfw ]}";
        };
      }
    );
}
