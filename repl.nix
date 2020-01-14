{ pkgs ? import nixpkgs (haskellNixpkgsArgs // { inherit system; })
# where
, nixpkgs ? haskellNix + "/nixpkgs"
, haskellNixpkgsArgs ? import haskellNix
, system ? builtins.currentSystem
# where
# ../haskell.nix at a99a0942284e63099116a44c4ccdec49be200184
, haskellNix ? #../haskell.nix
    builtins.fetchTarball {
        url = "https://github.com/input-output-hk/haskell.nix/archive/6c68f7d.tar.gz";
        # nix-prefetch-url --unpack https://github.com/input-output-hk/haskell.nix/archive/6c68f7d.tar.gz
        sha256 = "1161149aj7km2n6njb2dk71ddic8kjm4sszy7vfpigp1z3r31ysl";
    }
# ../plutus at 2b9e6493721aa5814698017a4e387dae2c3b2d8d
, plutus-src ? # ../plutus
    builtins.fetchTarball {
        url = "https://github.com/input-output-hk/plutus/archive/8f1117f8.tar.gz";
        # nix-prefetch-url --unpack https://github.com/input-output-hk/plutus/archive/8f1117f8.tar.gz
        sha256 = "06v4cz96bhm2w09gy0fr9jmc09g4f5rjyz2wjgwcl8vrhhv00mh6";
    }
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

    plutus-cache = [
        { name = "row-types";
          url = "https://github.com/target/row-types";
          rev = "1e8d5e084ffd46f6c7842826a1f62c60820885df";
          sha256 = "0ly5m4r8wkm8gdqyrqzsjfmp189yxsd4qp0zi3idrbgfaf45sk9k"; }
    ];

    # -------------------------------------------------------------------------
    # Building the plutus-plugin
    #
    # We need to build the plutus plugin for the host compiler, but it should be
    # the same compiler we build ghcjs with.  So be careful!

    plutus-plugin = (pkgs.haskell-nix.cabalProject {
        name = "plutus-plugin";
        index-state = "2019-12-13T00:00:00Z";
        # plan-sha256 = "0yxrmnviq058z48mw58z737xv98ldipx2ghghp42shzgk279dgg0";
        # materialized = ./materialized/plutus-plugin;
        src = plutus-src;

        cache = plutus-cache;
        pkg-def-extras = [(hackage: {
            packages = {
                # needed to build ghc-api-ghcjs properly. (has a <= 1.19.9 constraint)
                happy = hackage.happy."1.19.9".revisions.default;
            };
        })];
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
                packages.plutus-tx.flags = { ghcjs-plugin = true; };

                packages.ghc-api-ghcjs = import (pkgs.haskell-nix.callCabalToNix { name = "ghc-api-ghcjs"; src = "${ghcjs.configured-src}/lib/ghc-api-ghcjs"; });
            }
            {
                packages.ghc-api-ghcjs.flags.use-host-template-haskell = pkgs.lib.mkForce true;
            }
        ];
    }).plutus-tx.components.library;

    # create a separete package db, that containst he plutus-plugin as well as it's dependencies.  We can then just pass this
    # package database as `-host-package-db` to ghcjs when we need the plugin.
    plutus-plugin-pkg-db = pkgs.runCommand "plutus-plugin-pkg-db" { nativeBuildInputs = [ ghc ]; } ''
        ghc-pkg init $out/package.conf.d
        find "${plutus-plugin}/package.conf.d" -name "*.conf" -exec cp {} $out/package.conf.d \;
        find "${plutus-plugin.configFiles}/package.conf.d" -name "*.conf" -exec cp {} $out/package.conf.d \;
        ghc-pkg --package-db $out/package.conf.d recache
    '';

    # -------------------------------------------------------------------------
    # Building a plutus contract
    #
    # We need a patched Cabal 3+. So we'll build that first and then inject it
    # into the project for the SETUP_HS's to build against.  The fundamental
    # issue is that the Cabal version that ships with ghcjs is too old and too
    # restrictive (see the drop-pkg-db-check and no-final-check) patches.
    #
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

    # with the custom Cabal we can then build the plutus-use-cases
    # and do something like
    #
    # $ nix-build repl.nix -A plutus-use-cases.plutus-use-cases.components.exes.contract-guessing-game
    #
    # to build the guessing-game contract.
    #
    # We can then run the trace as follows:
    #
    # $ node result/bin/contract-guessing-game.jsexe/all.js trace guess
    #
    # or start the contract service like this:
    #
    # $ (cd result/bin/contract-guessing-game.jsexe && node /path/to/plutus/plutus-contract/platform/web/examples/server.js)
    #
    # at which point we can communicate with the service like so:
    #
    # $ curl localhost:16523/initialise | jq
    # {
    #   "newState": {
    #     "tag": "OpenRec",
    #     "contents": {
    #       "tag": "OpenBoth",
    #       "contents": [
    #         {
    #           "tag": "OpenBind",
    #           "contents": {
    #             "tag": "OpenLeaf",
    #             "contents": null
    #           }
    #         },
    #         {
    #           "tag": "OpenBind",
    #           "contents": {
    #             "tag": "OpenLeaf",
    #             "contents": null
    #           }
    #         }
    #       ]
    #     }
    #   },
    #   "hooks": {
    #     "slot": null,
    #     "lock": [
    #       "lock"
    #     ],
    #     "tx": [],
    #     "guess": [
    #       "guess"
    #     ],
    #     "own-pubkey": "NotWaitingForPubKey",
    #     "tx-confirmation": [],
    #     "address": [],
    #     "utxo-at": []
    #   }
    # }
    plutus-use-cases = (pkgs.pkgsCross.ghcjs.haskell-nix.cabalProject {
        name = "plutus-use-cases";
        src = plutus-src;
        index-state = "2019-12-13T00:00:00Z";
        # We'll need to set this to get `cabal configure` to produce the correct plan.
        # We need `--with-ghcjs=...` to make sure `cabal configure` doesn't complain about `ghcjs` missing.
        # event hough `--with-ghc` is specificed.
        configureArgs = "--ghcjs --with-ghcjs=js-unknown-ghcjs-ghc";
        # plan-sha256 = "1vc3zs5wqbxszf8896w56kd65877irbgl5k1dg2agmdh8zcjskmh";
        pkg-def-extras = [(hackage: {
            packages = {
                # needed for Cabal-3 compat.
                happy = hackage.happy."1.19.12".revisions.default;
                # https://github.com/input-output-hk/cardano-crypto/pull/62
                cardano-crypto = import (pkgs.haskell-nix.callCabalToNix { name = "cardano-crypto"; src = ./contrib/cardano-crypto; });
                # https://github.com/haskell-foundation/foundation/pull/531
                basement = import (pkgs.haskell-nix.callCabalToNix { name = "basement"; src = ./contrib/foundation/basement; });
                # https://github.com/haskell-foundation/foundation/pull/531
                foundation = import (pkgs.haskell-nix.callCabalToNix { name = "foundation"; src = ./contrib/foundation/foundation; });
                # https://github.com/biegunka/terminal-size/pull/12
                terminal-size = import (pkgs.haskell-nix.callCabalToNix { name = "terminal-size"; src = ./contrib/terminal-size; });
                # https://github.com/haskell/zlib/pull/25
                zlib = import (pkgs.haskell-nix.callCabalToNix { name = "zlib"; src = ./contrib/zlib; });

                # https://github.com/TomMD/entropy/pull/54
                entropy = import (pkgs.haskell-nix.callCabalToNix { name = "entropy"; src = ./contrib/entropy; });

                # https://github.com/luite/network at b77e7c10e29ccf3512be67defbe0fb66ac2bc4c8
                # this makes it compile, not necessarily "work".
                network = import (pkgs.haskell-nix.callCabalToNix { name = "network"; src = ./contrib/network; });

                # this is essential to build warp.
                # https://github.com/kazu-yamamoto/simple-sendfile/pull/34
                simple-sendfile = import (pkgs.haskell-nix.callCabalToNix { name = "simple-sendfile"; src = ./contrib/simple-sendfile; });

                # need these because for reasons I don't fully undertand yet, we fail to respect the freeze file.
                # FIXME: Maybe the cabalProject doesn't copy over the freeze file.
                servant-server = hackage.servant-server."0.15".revisions.default;
                servant-websockets = hackage.servant-websockets."1.1.0".revisions.default;
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
            # We need this module to depend on a newer `Cabal` version for the setups.
            {
                setup-depends = [ Cabal ];
            }
            {
                # required to build the contract and have ghcjs find the plutus plugin in the host package database
                packages.plutus-use-cases.configureFlags = [ "--ghcjs-option=-host-package-db=${plutus-plugin-pkg-db}/package.conf.d" ];
                packages.plutus-use-cases.setupBuildFlags = [ "--ghcjs-option=-host-package-db=${plutus-plugin-pkg-db}/package.conf.d" ];
            }
        ];
    });
}