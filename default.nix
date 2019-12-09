{ pkgs ? import nixpkgs ({
  inherit (haskellNixpkgsArgs) config;
  inherit system;
  overlays = haskellNixpkgsArgs.overlays ++ extraOverlays;
  })
# where
, nixpkgs ? haskellNix + "/nixpkgs"
, haskellNixpkgsArgs ? import haskellNix
, system ? builtins.currentSystem
# where
, haskellNix ? builtins.fetchTarball {
    url = "https://github.com/input-output-hk/haskell.nix/archive/bff6b2d3bf88cbad3b96f00a394a8bb69ec4fecb.tar.gz";
    sha256 = "0zzlmn3l9k2yqsy945izc96kki56kswsx7d1laqg1ak206bjmgg0";
  }

, haskellCompiler ? "ghc865"
, extraOverlays ? []
}:
let
  # the envionment hokey-pokey-env depends on.
  hokey-pokey-env = [ pkgs.haskell.compiler.ghcjs pkgs.haskell-nix.cabal-install node ];
  hokey-pokey-path = pkgs.lib.makeBinPath hokey-pokey-env;

  # the hokey-pokey-dot-cabal state.
  # if we need newer cabal packages, we need to bump this.
  hokey-pokey-dot-cabal = pkgs.haskell-nix.dotCabal {
      index-state = "2019-12-01T00:00:00Z";
      sha256 = "1akhhhp96qmr4qm7c7z2ij1cwkhw65rz912yl4k92764wgp3jpnd";
      inherit (pkgs.haskell-nix) cabal-install nix-tools;
    };

  # this will generate a standaline script, such that we don't need
  # to run all of this.  We will then embed it with the hokey-pokey service.
  hokey-pokey-drv-standalone = pkgs.writeText "hokey-pokey-standalone.nix" ''
  (import ./hokey-pokey-builder.nix) (import ${haskellNix + "/nixpkgs"} {}) "${hokey-pokey-path}" ${hokey-pokey-dot-cabal}
  '';

  node = pkgs.nodejs-12_x;
  project = pkgs.haskell-nix.cabalProject {
    src = pkgs.haskell-nix.haskellLib.cleanGit { src = ./.; };
    ghc = pkgs.haskell-nix.compiler.${haskellCompiler};
    modules = [{
      packages.hokey-pokey.postInstall = ''
        # embed the .nix scripts into the dataDir. May god have mercy on my soul for I have sinned...
        DATADIR=$(find $out/share -name "defaultProject" -type d)/..
        cp ${hokey-pokey-drv-standalone} $DATADIR/hokey-pokey-drv-standalone.nix
      '';
      packages.hokey-pokey.components.tests.hokey-pokey-test.preCheck = ''
        export HOME=$(mktemp -d)
        mkdir $HOME/.cabal
        ln -s ${pkgs.haskell-nix.dotCabal {
          index-state = "2019-12-01T00:00:00Z";
          sha256 = "1akhhhp96qmr4qm7c7z2ij1cwkhw65rz912yl4k92764wgp3jpnd";
          inherit (pkgs.haskell-nix) cabal-install nix-tools;
        }}/.cabal/* $HOME/.cabal
      '';
    }];
  };
  addCabalInstall = drv: drv.overrideAttrs (oldAttrs: {
    buildInputs = (oldAttrs.buildInputs or []) ++
      [ pkgs.haskell-nix.cabal-install ];
  });
  addNix = drv: drv.overrideAttrs (oldAttrs: {
    buildInputs = (oldAttrs.buildInputs or []) ++
      [ pkgs.nix ];
  });
  addGhcjsAndNode = drv: drv.overrideAttrs (oldAttrs: {
    # Use nispkgs ghcjs for now
    buildInputs = (oldAttrs.buildInputs or []) ++
      [ pkgs.haskell.compiler.ghcjs node ];
  });

in project // rec {
  inherit pkgs;
  hokey-pokey-drv-json = (import ./data/hokey-pokey-builder-json.nix) pkgs hokey-pokey-path hokey-pokey-dot-cabal;

  # test the hokey-pokey-derivation generator.
  hokey-pokey-drv-test = hokey-pokey-drv-json (builtins.readFile ./test/test.json);

  # wrap hokey-pokey
  hokey-pokey-wrapped = project.hokey-pokey.components.exes.hokey-pokey.overrideAttrs (oldAttrs: {
    nativeBuildInputs = oldAttrs.nativeBuildInputs ++ [ pkgs.makeWrapper ];
    postInstall = ''
        # embed the .nix scripts into the dataDir. May god have mercy on my soul for I have sinned...
        DATADIR=$(find $out/share -name "defaultProject" -type d)/..
        cp ${hokey-pokey-drv-standalone} $DATADIR/hokey-pokey-drv-standalone.nix
        wrapProgram $out/bin/hokey-pokey --prefix PATH ":" ${pkgs.lib.makeBinPath [ pkgs.nix ]}
      '';
  });
  shell = addGhcjsAndNode (project.shellFor {});
  checks = pkgs.recurseIntoAttrs {
    hokey-pokey-test = addNix
      (addGhcjsAndNode project.hokey-pokey.checks.hokey-pokey-test);
  };
}
