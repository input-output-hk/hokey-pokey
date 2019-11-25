{ pkgs ? import nixpkgs haskellNixpkgsArgs
, nixpkgs ? haskellNix + "/nixpkgs"
, haskellNixpkgsArgs ? import haskellNix
, haskellNix ? builtins.fetchTarball {
    url = "https://github.com/input-output-hk/haskell.nix/archive/c2be4abc81899d7f61b23eeb1db7ffe56989db8f.tar.gz";
    sha256 = "1dyvqgglsmqw80i8qa0xh375ixhwzd1im476l4s9l690rddg10bv";
  }
, haskellCompiler ? "ghc865"
, system ? null
}:
  pkgs.haskell-nix.cabalProject {
    src = pkgs.haskell-nix.haskellLib.cleanGit { src = ./.; };
    ghc = pkgs.haskell-nix.compiler.${haskellCompiler};
  }
