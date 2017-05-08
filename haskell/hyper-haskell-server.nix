############################################################################
# Nix derivation for the server back-end of the
# HyperHaskell graphical Haskell interpreter.
#
# Install this file with:
#   nix-env -if haskell/hyper-haskell-server.nix
#
# Then run the application with:
#   nix-shell -p electron --run "electron app"
#
# Remember to set the Interpreter Back-end to "nix" in Settings,
# and then Ctrl-R to Reload imports.
#
# Other haskell packages can be added to the environment by using the
# --arg extra parameter of nix-env.
#
############################################################################

{ pkgs ? import <nixpkgs> {}, extra ? ["hyper-extra"] }:

let
  executableHaskellDepends = ps: with ps; [
      aeson base bytestring deepseq exceptions hint hyper scotty text
      transformers
    ] ++ (map (p: ps.${p}) extra);

  ghc = pkgs.haskellPackages.ghcWithPackages executableHaskellDepends;

  drv = ghc.haskellPackages.mkDerivation {
    pname = "hyper-haskell-server";
    version = "0.1.0.1";
    sha256 = "43b0d770896ca0c38aee876bb23ee03b20009ce7afab4d6b5ca07a99f6e7f290";
    isLibrary = false;
    isExecutable = true;
    executableHaskellDepends = executableHaskellDepends ghc.haskellPackages;
    description = "Server back-end for the HyperHaskell graphical Haskell interpreter";
    license = pkgs.stdenv.lib.licenses.bsd3;

    buildTools = [ pkgs.makeWrapper ];
    postInstall = ''
      wrapProgram "$out/bin/hyper-haskell-server" \
          --set NIX_GHC ${ghc}/bin/ghc \
          --set NIX_GHCPKG ${ghc}/bin/ghc-pkg \
          --set NIX_GHC_LIBDIR ${ghc}/lib/ghc-8.0.2
    '';
  };

in
  drv
