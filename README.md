# hokey-pokey

### Building in a nix-shell

```
nix-shell
cabal build all
```

### Building with nix

```
nix build -f default.nix hokey-pokey.components.exes.hokey-pokey
```
