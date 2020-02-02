{ pkgs ? import nixpkgs (haskellNixpkgsArgs // { inherit system; })
# where
, nixpkgs ? haskellNix + "/nixpkgs"
, haskellNixpkgsArgs ? import haskellNix
, system ? builtins.currentSystem
# where
# ../haskell.nix at a99a0942284e63099116a44c4ccdec49be200184
, haskellNix ? #../haskell.nix
    builtins.fetchTarball {
        url = "https://github.com/input-output-hk/haskell.nix/archive/dbc8deae1057776fa6c786c48a35967a2c560358.tar.gz";
        # nix-prefetch-url --unpack https://github.com/input-output-hk/haskell.nix/archive/dbc8deae1057776fa6c786c48a35967a2c560358.tar.gz
        sha256 = "057wjisqvmdfn1wyhdbw0cs5r03vypz2cvh3hdapiz4xi3x2gj67";
    }
# ../plutus at 2b9e6493721aa5814698017a4e387dae2c3b2d8d
, plutus-src ? #../plutus
    builtins.fetchTarball {
        url = "https://github.com/input-output-hk/plutus/archive/6fb5c77d.tar.gz";
        # nix-prefetch-url --unpack https://github.com/input-output-hk/plutus/archive/6fb5c77d.tar.gz
        sha256 = "0a09k85ibs4bx2vrg6r911f2lnf4zd5rwd49mx1jswi1dvxpxkrm";
    }
, haskellCompiler ? "ghc865"
}: rec {
    shell = pkgs.mkShell {
        buildInputs = [
          pkgs.pkgsCross.ghcjs.buildPackages.haskell-nix.compiler.ghc865
          pkgs.haskell-nix.snapshots."lts-14.20".happy.components.exes.happy
          pkgs.haskell-nix.snapshots."lts-14.20".alex.components.exes.alex
          cabal-project.hsPkgs.cabal-install.components.exes.cabal
        ];
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
    plutus-plugin-pkg-db = (pkgs.runCommand "plutus-plugin-pkg-db" { nativeBuildInputs = [ ghc ]; } ''
        ghc-pkg init $out/lib/ghc-8.6.5/package.conf.d
        find "${plutus-plugin}/package.conf.d" -name "*.conf" -exec cp {} $out/lib/ghc-8.6.5/package.conf.d \;
        find "${plutus-plugin.configFiles}/lib/ghc-8.6.5/package.conf.d" -name "*.conf" -exec cp {} $out/lib/ghc-8.6.5/package.conf.d \;
        ghc-pkg --package-db $out/lib/ghc-8.6.5/package.conf.d recache
    '') + "/lib/ghc-8.6.5";

    # -------------------------------------------------------------------------
    # Building a plutus contract
    #
    # We need a patched Cabal 3+. So we'll build that first and then inject it
    # into the project for the SETUP_HS's to build against.  The fundamental
    # issue is that the Cabal version that ships with ghcjs is too old and too
    # restrictive (see the drop-pkg-db-check and no-final-check) patches.
    #
    cabal-project = pkgs.haskell-nix.hackage-project {
        name = "cabal-install"; version = "3.0.0.0";
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
                setup-depends = [ cabal-project.hsPkgs.Cabal ];
            }
            {
                # required to build the contract and have ghcjs find the plutus plugin in the host package database
                packages.plutus-use-cases.configureFlags = [ "--ghcjs-option=-host-package-db=${plutus-plugin-pkg-db}/package.conf.d" ];
                packages.plutus-use-cases.setupBuildFlags = [ "--ghcjs-option=-host-package-db=${plutus-plugin-pkg-db}/package.conf.d" ];
            }
        ];
    });
    # To try out the shell do (where ../plutus is the angerman/ghcjs-shell branch of plutus):
    #   nix-shell repl.nix -A plutus-use-cases-shell
    #   cd ../plutus/plutus-use-cases
    #   echo 'packages: .' > cabal.project
    #   cabal new-build --ghcjs --with-ghcjs=js-unknown-ghcjs-ghc --with-ghcjs-pkg=js-unknown-ghcjs-ghc-pkg --hsc2hs-option=--cross-compile --ghcjs-option=-host-package-db=$PLUTUS_PLUGIN_PKG_DB/package.conf.d plutus-use-cases:lib:plutus-use-cases
    plutus-use-cases-shell = (plutus-use-cases.shellFor {
      packages = ps: [ ps.plutus-use-cases ];
      withHoogle = false;
    }).overrideAttrs (drv: {
      PLUTUS_PLUGIN_PKG_DB=plutus-plugin-pkg-db;
    });
}
