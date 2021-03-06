{ pkgs ? import nixpkgs (haskellNixpkgsArgs // { inherit system; })
# where
, nixpkgs ? haskellNix + "/nixpkgs"
, haskellNixpkgsArgs ? import haskellNix
, system ? builtins.currentSystem
# where
, haskellNix ? builtins.fetchTarball {
    url = "https://github.com/input-output-hk/haskell.nix/archive/f5d1f8.tar.gz";
    # nix-prefetch-url --unpack https://github.com/input-output-hk/haskell.nix/archive/f5d1f8.tar.gz
    sha256 = "1720vv0958jhpjkndf7pbb1dc4g5rvycfqqxbarcramiv1ybs4gk";
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
in project // rec {
  inherit pkgs;
  hokey-pokey-env = [ pkgs.haskell.compiler.ghcjs pkgs.haskell-nix.cabal-install node ];
  hokey-pokey-wrapped = project.hokey-pokey.components.exes.hokey-pokey.overrideAttrs (oldAttrs: {
    nativeBuildInputs = oldAttrs.nativeBuildInputs ++ [ pkgs.makeWrapper ];
    postInstall = ''
        wrapProgram $out/bin/hokey-pokey --prefix PATH ":" ${pkgs.lib.makeBinPath hokey-pokey-env }
      '';
  });
  shell = addGhcjsAndNode (project.shellFor {});
  checks = pkgs.recurseIntoAttrs {
    hokey-pokey-test = addCabalInstall
      (addGhcjsAndNode project.hokey-pokey.checks.hokey-pokey-test);
  };
}
