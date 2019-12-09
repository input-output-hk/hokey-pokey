{ pkgs ? import nixpkgs (haskellNixpkgsArgs // { inherit system; })
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
}:
let
  node = pkgs.nodejs-12_x;
  project = pkgs.haskell-nix.cabalProject {
    src = pkgs.haskell-nix.haskellLib.cleanGit { src = ./.; };
    ghc = pkgs.haskell-nix.compiler.${haskellCompiler};
    modules = [{
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
  addGhcjsAndNode = drv: drv.overrideAttrs (oldAttrs: {
    # Use nispkgs ghcjs for now
    buildInputs = (oldAttrs.buildInputs or []) ++
      [ pkgs.haskell.compiler.ghcjs node ];
  });
in project // {
  inherit pkgs;
  hokey-pokey-env = [ pkgs.haskell.compiler.ghcjs pkgs.haskell-nix.cabal-install node ];
  shell = addGhcjsAndNode (project.shellFor {});
  checks = pkgs.recurseIntoAttrs {
    hokey-pokey-test = addCabalInstall
      (addGhcjsAndNode project.hokey-pokey.checks.hokey-pokey-test);
  };
}
