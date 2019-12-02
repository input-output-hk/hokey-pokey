builtins.mapAttrs (k: _v:
  let
    project = import ./. { system = k; };
    pkgs = project.pkgs;
  in
  pkgs.recurseIntoAttrs {
    inherit (project) checks;
  }
) {
  x86_64-linux = {};


  # Uncomment to test build on macOS too
  # x86_64-darwin = {};
}
