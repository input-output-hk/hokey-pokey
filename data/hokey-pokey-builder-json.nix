pkgs: env: dot-cabal: jsonSrc: pkgs.stdenv.mkDerivation {
    name = "hokey-pokey-build";
    phases = [ "unpackPhase" "buildPhase" ];
    unpackPhase = ''
    mkdir src
    cd src
    '' + (pkgs.lib.concatMapStrings ({ filename, contents }: ''
    cat > ${filename} <<EOF
    ${contents}
    EOF
    '') (builtins.fromJSON jsonSrc));

    buildPhase = ''
    export PATH=${env}:$PATH
    export HOME=$(mktemp -d)
    mkdir $HOME/.cabal
    ln -s ${dot-cabal}/.cabal/* $HOME/.cabal

    cabal build --ghcjs exe:test
    find dist-newstyle/build -name "all.js" -path "*/x/*" -exec cp {} $out \;
    '';
}