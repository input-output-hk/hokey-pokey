{ pkgs ? import nixpkgs haskellNixpkgsArgs
, nixpkgs ? haskellNix + "/nixpkgs"
, haskellNixpkgsArgs ? import haskellNix
, haskellNix ? builtins.fetchTarball {
    url = "https://github.com/input-output-hk/haskell.nix/archive/bff6b2d3bf88cbad3b96f00a394a8bb69ec4fecb.tar.gz";
    sha256 = "0zzlmn3l9k2yqsy945izc96kki56kswsx7d1laqg1ak206bjmgg0";
  }
, haskellCompiler ? "ghc865"
, system ? null
}:
let
  node = pkgs.nodejs-12_x;
  project = pkgs.haskell-nix.cabalProject {
    src = pkgs.haskell-nix.haskellLib.cleanGit { src = ./.; };
    ghc = pkgs.haskell-nix.compiler.${haskellCompiler};
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
  shell = addGhcjsAndNode (project.shellFor {});
  checks = pkgs.recurseIntoAttrs {
    hokey-pokey-test = addCabalInstall
      (addGhcjsAndNode project.hokey-pokey.checks.hokey-pokey-test);
  };
}
