{ sources ? (import ./nix/sources.nix) # // { "haskell.nix" = ../haskell.nix; }
, pkgs ? import sources.nixpkgs (haskellNixpkgsArgs // { inherit system; })
# where
, nixpkgs ? sources.nixpkgs
, haskellNixpkgsArgs ? (import haskellNix {}).nixpkgsArgs
, system ? builtins.currentSystem
, haskellNix ? sources."haskell.nix"
, plutus-src ? # pkgs.haskell-nix.haskellLib.cleanGit { src = ../plutus; }
               sources.plutus
, haskellCompiler ? "ghc883"
}: rec {
    shell = pkgs.mkShell {
        buildInputs = [
          pkgs.pkgsCross.ghcjs.buildPackages.haskell-nix.compiler.ghc883
          pkgs.haskell-nix.snapshots."lts-14.20".happy.components.exes.happy
          pkgs.haskell-nix.snapshots."lts-14.20".alex.components.exes.alex
          cabal-project.hsPkgs.cabal-install.components.exes.cabal
        ];
    };
    ghcjs = pkgs.pkgsCross.ghcjs.buildPackages.haskell-nix.compiler.ghc883;
    ghc = pkgs.haskell-nix.compiler.ghc883;
    shell-env = [
        pkgs.pkgsCross.ghcjs.buildPackages.haskell-nix.compiler.ghc883
        pkgs.haskell-nix.compiler.ghc883
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
        ghc = pkgs.haskell-nix.compiler.ghc883;
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
                packages.plutus-tx-plugin.flags = { ghcjs-plugin = true; };

                packages.ghc-api-ghcjs = import (pkgs.haskell-nix.callCabalToNix { name = "ghc-api-ghcjs"; src = "${ghcjs.configured-src}/lib/ghc-api-ghcjs"; });
            }
            {
                packages.ghc-api-ghcjs.flags.use-host-template-haskell = pkgs.lib.mkForce true;
            }
        ];
    });

    # create a separete package db, that containst he plutus-plugin as well as it's dependencies.  We can then just pass this
    # package database as `-host-package-db` to ghcjs when we need the plugin.
    plutus-plugin-pkg-db-configFiles = plutus-plugin.makeConfigFiles {
      fullName = "plutus-plugin-db";
      identifier.name = "plutus-plugin-db";
      ghc = pkgs.haskell-nix.compiler.ghc883;
      component = {
        depends = [ plutus-plugin.plutus-tx-plugin ];
        libs = [];
        frameworks = [];
        doExactConfig = false;
      };
    };
    plutus-plugin-pkg-db = plutus-plugin-pkg-db-configFiles + "/${plutus-plugin-pkg-db-configFiles.packageCfgDir}";

    # -------------------------------------------------------------------------
    # Building a plutus contract
    #
    # We need a patched Cabal 3+. So we'll build that first and then inject it
    # into the project for the SETUP_HS's to build against.  The fundamental
    # issue is that the Cabal version that ships with ghcjs is too old and too
    # restrictive (see the drop-pkg-db-check and no-final-check) patches.
    #
    cabal-project = pkgs.haskell-nix.hackage-project {
        name = "cabal-install"; version = "3.2.0.0";
        ghc = pkgs.haskell-nix.compiler.ghc883;
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
                packages.plutus-ledger.configureFlags = [ "--ghcjs-option=-host-package-db=${plutus-plugin-pkg-db}" ];
                packages.plutus-ledger.setupBuildFlags = [ "--ghcjs-option=-host-package-db=${plutus-plugin-pkg-db}" ];
                packages.plutus-use-cases.configureFlags = [ "--ghcjs-option=-host-package-db=${plutus-plugin-pkg-db}" ];
                packages.plutus-use-cases.setupBuildFlags = [ "--ghcjs-option=-host-package-db=${plutus-plugin-pkg-db}" ];
                packages.network.configureFlags = [ "--configure-option=--host=x86_64-linux" ];
                packages.plutus-tx-plugin.doHaddock = false;
            }
        ];
    });
    plutus-use-cases-ghc = pkgs.haskell-nix.cabalProject {
        name = "plutus-use-cases";
        index-state = "2020-04-05T00:00:00Z";
        ghc = pkgs.haskell-nix.compiler.ghc883;
        src = plutus-src;
#        pkg-def-extras = [(hackage: {
#            packages = {
#                # needed to build ghc-api-ghcjs properly. (has a <= 1.19.9 constraint)
#                happy = hackage.happy."1.19.9".revisions.default;
#            };
#        })];
        modules = [
            {
                bootPkgs = [ "ghcjs-prim" ];
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
#                packages.plutus-tx-plugin.flags = { ghcjs-plugin = true; };

#                packages.ghc-api-ghcjs = import (pkgs.haskell-nix.callCabalToNix { name = "ghc-api-ghcjs"; src = "${ghcjs.configured-src}/lib/ghc-api-ghcjs"; });
                packages.plutus-ledger.doHaddock = false;
            }
            {
                packages.ghc-api-ghcjs.flags.use-host-template-haskell = pkgs.lib.mkForce true;
            }
            # We need this module to depend on a newer `Cabal` version for the setups.
            {
                setup-depends = [ cabal-project.hsPkgs.Cabal ];
            }
        ];
    };
    plutus-use-cases-ghc-shell = (plutus-use-cases-ghc.shellFor {
      packages = ps: [ ps.plutus-use-cases ];
    }).overrideAttrs (attrs: {
      buildInputs = [
        ghcide
        cabal-project.hsPkgs.cabal-install.components.exes.cabal
        pkgs.binutils-unwrapped
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
    # This is a wrapper that workd outside of a nix-shell.
    #   $(nix-build ../../hokey-pokey/repl.nix -A plutusc)/bin/plutusc src/Language/PlutusTx/Coordination/Contracts.hs src/Language/PlutusTx/Coordination/Contracts/Game.hs exe/game/Main.hs
    plutusc = pkgs.runCommand "plutusc-wrapper" {
      buildInputs = [
        pkgs.makeWrapper
      ];
    } ''
      mkdir -p $out/bin
      makeWrapper ${plutus-use-cases-shell.ghc}/bin/js-unknown-ghcjs-ghc $out/bin/plutusc \
          --add-flags '"-host-package-db=${plutus-plugin-pkg-db}"'
    '';
    # Docker conatainer built with nix.
    #   docker load < $(nix-build repl.nix -A plutus-dockerc)
    #   cd ../plutus/plutus-use-cases
    #   docker run --rm -it -v$(pwd):/build -w /build plutusc plutusc src/Language/PlutusTx/Coordination/Contracts.hs src/Language/PlutusTx/Coordination/Contracts/Game.hs exe/game/Main.hs
    plutusc-docker = pkgs.dockerTools.buildImage {
      name = "plutusc";
      tag = "latest";
      contents = [ plutus-use-cases-shell.ghc ];
      extraCommands = "mkdir -m 0777 tmp";
      config = {
        Env = [
          "PATH=${plutusc}/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"
        ];
      };
    };
    # A bundle for macOS and linux systems like ubuntu (not NixOS) that can be relocated
    # To test on macOS install gcc and node then
    #   cd ../plutus/plutus-use-cases
    #   mkdir plutusc
    #   cd plutusc
    #   tar -xzf $(nix-build ../../../plutus-compilation-service/repl.nix -A plutusc-bundle-tar)
    #   cd ..
    #   PATH=$(pwd)/plutusc/bin:$PATH plutusc -package plutus-wallet-api src/Language/PlutusTx/Coordination/Contracts.hs src/Language/PlutusTx/Coordination/Contracts/Game.hs exe/game/Main.hs
    # Same steps should work on ubuntu or you can use ubuntu-plutusc-docker (defined bellow)
    plutusc-bundle = ghcjs.bundled-ghcjs {
      compilerName = "plutusc";
      db = plutus-use-cases-shell.configFiles;
      hostDb = plutus-plugin-pkg-db-configFiles;
    };
    # Tarball of plutusc-bundle
    plutusc-bundle-tar = pkgs.runCommand "plutusc-bundle.tar.gz" {} ''
      cd ${plutusc-bundle}
      tar -chzf $out .
    '';
    # Base image for testing plutusc-bundle on Ubuntu
    ubuntu = pkgs.dockerTools.pullImage {
      imageName = "ubuntu";
      imageDigest = "sha256:bd5f4f235eb31768b2c5caf1988bbdc182d4fc3cb6ee4aca6c6d74613f256140";
      sha256 = "150nd664hnkzb81v61ycq761yxvjrz7m63p754zwif4vzjxqfywk";
      finalImageTag = "19.10";
      finalImageName = "ubuntu";
    };
    plutusc-bundle-unpacked = pkgs.runCommand "plutusc-bundle-unpacked" {} ''
      mkdir -p $out/plutusc
      cd $out/plutusc
      tar -xzf ${plutusc-bundle-tar}
    '';
    # Docker image that we can use to test that plutusc-bundle works on ubuntu.
    # We use a Dockerfile to add gcc and node to the image because
    # you cannot `apt-get install` stuff with `buildImage`.
    #   docker load < $(nix-build repl.nix -A ubuntu-plutusc-docker)
    #   docker build -t test-ubuntu-plutusc ./test-ubuntu-plutusc
    #   cd ../plutus/plutus-use-cases
    #   docker run --rm -it -v$(pwd):/build -w /build test-ubuntu-plutusc plutusc src/Language/PlutusTx/Coordination/Contracts.hs src/Language/PlutusTx/Coordination/Contracts/Game.hs exe/game/Main.hs
    ubuntu-plutusc-docker = pkgs.dockerTools.buildImage {
      name = "ubuntu-plutusc";
      tag = "latest";
      fromImage = ubuntu;
      contents = [plutusc-bundle-unpacked];
      config = {
        Env = [ "PATH=/plutusc/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin" ];
      };
    };
    devcontainer = (import sources.docker-nixpkgs).devcontainer;
    ghcide = (import (sources.ghcide-nix + "/nix") { inherit sources system; }).export.ghcide-ghc883;

    # WIP to make a VS Code devcontainer that can be used for working on plutus code
    #   docker load < $(nix-build repl.nix --system x86_64-linux -A plutusc-devcontainer)
    # In VS Code install:
    #   https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-containers
    # Follow these instructions from step 2:
    #   https://code.visualstudio.com/docs/remote/containers#_quick-start-try-a-dev-container
    #   (the folder you should open is the plutus one).
    plutusc-devcontainer = pkgs.dockerTools.buildImage {
      name = "plutusc-devcontainer";
      tag = "latest";
      fromImage = devcontainer;
      contents = [
        plutus-use-cases-shell.ghc
        plutus-use-cases-ghc-shell.ghc
        ghcide
        cabal-project.hsPkgs.cabal-install.components.exes.cabal
        pkgs.binutils-unwrapped
      ];
      extraCommands = "mkdir -m 0777 tmp";
      config = {
        Env = [
        "ENV=/nix/var/nix/profiles/default/etc/profile.d/nix.sh"
        "GIT_SSL_CAINFO=/nix/var/nix/profiles/default/etc/ssl/certs/ca-bundle.crt"
        "LD_LIBRARY_PATH=/nix/var/nix/profiles/default/lib"
        "PAGER=less"
        "PATH=/nix/var/nix/profiles/default/bin:${plutusc}/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"
        "SSL_CERT_FILE=/nix/var/nix/profiles/default/etc/ssl/certs/ca-bundle.crt"
        ];
      };
    };
}
