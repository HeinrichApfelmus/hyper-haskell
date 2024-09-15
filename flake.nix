{
  description = ''HyperHaskell'';

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:hamishmack/flake-utils/hkm/nested-hydraJobs";
  };

  outputs = inputs:
    let
      supportedSystems = [
        "x86_64-linux"
        "x86_64-darwin"
        "aarch64-linux"
        "aarch64-darwin"
       ]; in
    inputs.flake-utils.lib.eachSystem supportedSystems (system:
      let
        # ###########################################
        # Imports

        # Packages with patches
        pkgs = inputs.nixpkgs.legacyPackages.${system};

        # Library
        lib = pkgs.lib;
        
        # ###########################################
        # Helpers

      in rec {
        packages = { };

        apps = { };

        devShells.default = pkgs.mkShell {
          buildInputs = [
            pkgs.just

            pkgs.ghc
            pkgs.pkg-config
            pkgs.zlib

            pkgs.electron
          ] ++ lib.optionals pkgs.stdenv.isDarwin ([
            pkgs.darwin.apple_sdk.frameworks.Cocoa
            pkgs.darwin.apple_sdk.frameworks.CoreServices
            pkgs.darwin.objc4.all
          ]);
        };

        shellHook = ''
            # add shell hook here
          '';
      }
    );
}
