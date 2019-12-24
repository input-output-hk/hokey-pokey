{ pkgs ? import nixpkgs (haskellNixpkgsArgs // { inherit system; })
# where
, nixpkgs ? haskellNix + "/nixpkgs"
, haskellNixpkgsArgs ? import haskellNix
, system ? builtins.currentSystem
# where
, haskellNix ? ../haskell.nix
# builtins.fetchTarball {
#     url = "https://github.com/input-output-hk/haskell.nix/archive/f5d1f8.tar.gz";
#     # nix-prefetch-url --unpack https://github.com/input-output-hk/haskell.nix/archive/f5d1f8.tar.gz
#     sha256 = "1720vv0958jhpjkndf7pbb1dc4g5rvycfqqxbarcramiv1ybs4gk";
#   }
, plutus-src ? ../plutus #builtins.fetchTarball {
#    url = "https://github.com/luite/plutus/archive/5faa07a.tar.gz";
#    sha256 = "1q9lpqda65hwxq22xd7iizdqipn7vafhpbsr55dzzvcy6cpajbcw";
#}
, haskellCompiler ? "ghc865"
}: rec {
    shell = pkgs.mkShell {
        buildInputs = [  pkgs.pkgsCross.ghcjs.buildPackages.haskell-nix.compiler.ghc865 ];
    };
    ghcjs = pkgs.pkgsCross.ghcjs.buildPackages.haskell-nix.compiler.ghc865;
    ghc = pkgs.haskell-nix.compiler.ghc865;
    shell-env = [
        pkgs.pkgsCross.ghcjs.buildPackages.haskell-nix.compiler.ghc865
        pkgs.haskell-nix.compiler.ghc865
        pkgs.haskell-nix.cabal-install
        pkgs.git
        pkgs.haskell-nix.nix-tools

        # to build ghcjs
        pkgs.nodejs
        pkgs.gcc
        pkgs.gmp
        pkgs.gmp.dev
        pkgs.makeWrapper
        pkgs.xorg.lndir
        pkgs.pkgconfig
    ];
    # plutus-src = pkgs.runCommand "plutus-ghcjs-src" {} ''
    # mkdir $out
    # cp -fR ${plutus} $out/plutus
    # cp -fR ${ghcjs.configured-src} $out/ghcjs
    # cp ${./plutus.cabal.project} $out/cabal.project
    # '';
    plutus-plugin = (pkgs.haskell-nix.cabalProject {
        src = plutus-src;
        modules = [
            {
                # generate with something like:   for pkg in $(ghc-pkg list|grep -v "package.conf" |xargs); do echo -n " \"${pkg%-*}\""; done
                # in a ghc shell. E.g. in   nix-shell --pure -p '[ (import ./repl.nix {}).ghc ]'
                # will need a minor massaging to get into this form:
                nonReinstallablePkgs = [ "Cabal" "array" "base" "binary" "bytestring" "containers" "deepseq"
                                         "directory" "filepath" "ghc" "ghc-boot" "ghc-boot-th" "ghc-compact"
                                         "ghc-heap" "ghc-prim" "ghci" "haskeline" "hpc" "integer-gmp"
                                         "libiserv" "mtl" "parsec" "pretty" "process" "rts" "stm"
                                         "template-haskell" "terminfo" "text" "time" "transformers" "unix"
                                         "xhtml"
                                         ];
                # # we need ghc-boot in here for ghcjs.
                # nonReinstallablePkgs = [ "rts" "ghc-heap" "ghc-prim" "integer-gmp" "integer-simple" "base"
                #                          "deepseq" "array" "ghc-boot-th" "pretty" "template-haskell"

                #                          "ghc-boot" "binary" "bytestring" "filepath" "directory" "containers"
                #                          "time" "unix" "Win32" "ghc" "ghci"];
            }
            {
                packages.Cabal.patches = [ "${haskellNix}/overlays/patches/Cabal/fix-data-dir.patch" ];
                packages.ghc.flags.ghci = pkgs.lib.mkForce true;
                packages.ghci.flags.ghci = pkgs.lib.mkForce true;
            }
        ];
    }).plutus-tx.components.library;
    plutus-plugin-pkg-db = pkgs.runCommand "plutus-plugin-pkg-db" { nativeBuildInputs = [ ghc ]; } ''
        ghc-pkg init $out/package.conf.d
        find "${plutus-plugin}/package.conf.d" -name "*.conf" -exec cp {} $out/package.conf.d \;
        find "${plutus-plugin.configFiles}/package.conf.d" -name "*.conf" -exec cp {} $out/package.conf.d \;
        ghc-pkg --package-db $out/package.conf.d recache
    '';
    Cabal = pkgs.haskell-nix.hackage-package {
        name = "Cabal"; version = "3.0.0.0";
        modules = [
            { packages.Cabal.patches = [
                ./Cabal-3.0.0.0-drop-pkg-db-check.diff
                ./Cabal-3.0.0.0-no-final-checks.diff
            ]; }
            {
                nonReinstallablePkgs = [ "array" "base" "binary" "bytestring" "containers" "deepseq"
                                         "directory" "filepath" "ghc" "ghc-boot" "ghc-boot-th" "ghc-compact"
                                         "ghc-heap" "ghc-prim" "ghci" "haskeline" "hpc" "integer-gmp"
                                         "libiserv" "mtl" "parsec" "pretty" "process" "rts" "stm"
                                         "template-haskell" "terminfo" "text" "time" "transformers" "unix"
                                         "xhtml"
                                         ];
            }
        ];
    };
    plutus-use-cases = (pkgs.pkgsCross.ghcjs.haskell-nix.cabalProject {
        name = "plutus-use-cases";
        src = plutus-src;
        index-state = "2019-12-13T00:00:00Z";
        # plan-sha256 = "1vc3zs5wqbxszf8896w56kd65877irbgl5k1dg2agmdh8zcjskmh";
        pkg-def-extras = [(hackage: {
            packages = {
                # needed for Cabal-3 compat.
                happy = hackage.happy."1.19.12".revisions.default;
                # cryptonite-openssl = import (pkgs.haskell-nix.callCabalToNix { name = "cryptonite-openssl"; src = ./contrib/cryptonite-openssl; });
                cardano-crypto = import (pkgs.haskell-nix.callCabalToNix { name = "cardano-crypto"; src = ./contrib/cardano-crypto; });
                basement = import (pkgs.haskell-nix.callCabalToNix { name = "basement"; src = ./contrib/foundation/basement; });
                foundation = import (pkgs.haskell-nix.callCabalToNix { name = "foundation"; src = ./contrib/foundation/foundation; });
                terminal-size = import (pkgs.haskell-nix.callCabalToNix { name = "terminal-size"; src = ./contrib/terminal-size; });
            };
        })];

        modules = [
            {
                bootPkgs = [ "ghcjs-prim" ];
                nonReinstallablePkgs = [
                    "Cabal"
                    "array" "base" "binary" "bytestring" "containers" "deepseq"
                    "directory" "filepath" "ghc" "ghc-boot" "ghc-boot-th" "ghc-compact"
                    "ghc-heap" "ghc-prim" "ghci" "ghcjs-prim" "ghcjs-th" "integer-gmp"
                    "mtl" "parsec" "pretty" "process" "rts" "template-haskell" "text"
                    "time" "transformers" "unix"

                    "hpc"

                    # we can't build this one, so let's pretend it pre-exists.
                    "terminfo"

                    # This one is just absolutely broken.
                    "cabal-doctest"
                ];
            }
            {
                setup-depends = [ Cabal ];
            }
        ];
    });
}