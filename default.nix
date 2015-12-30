{ stdenv, haskellPackages, pkgs, profiling ? false}:

let
  haskellEnv = haskellPackages.ghcWithPackages (p: with p; [
    cabal-install
    hex
  ]);
  env = (with pkgs; [
    gdb
  ]);
in
  stdenv.mkDerivation {
    name        = "lz4hs";
    buildInputs = [
      haskellEnv
      env
   ];
    shellHook   = ''
      echo env "${haskellEnv}"
      export PS1="\[\033[1;32m\][\h \W]\$\[\033[0m\] "
      export PS2=">"
      export PROJECT_PATH=$(pwd)
      export PATH=$PATH:$PROJECT_PATH
      export NIX_GHC="${haskellEnv}/bin/ghc"
      export NIX_GHCPKG="${haskellEnv}/bin/ghc-pkg"
      export NIX_GHC_DOCDIR="${haskellEnv}/share/doc/ghc/html"
      export NIX_GHC_LIBDIR=$( $NIX_GHC --print-libdir )
      alias hs="runghc"
      '';
  }
