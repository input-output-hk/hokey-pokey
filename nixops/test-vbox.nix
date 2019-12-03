{
  test-server =
    { config, pkgs, ... }:
    { deployment.targetEnv = "virtualbox";
      deployment.virtualbox.memorySize = 1024; # megabytes
      deployment.virtualbox.vcpu = 2; # number of cpus
      # deployment.keys = import ./localhost-keys.nix;
      nixpkgs.localSystem.system = "x86_64-linux";
      services.nginx = {
        enable = true;
        virtualHosts."local.hokey-pokey" = {
          default = true;
          locations."/" = {
            proxyPass = "http://127.0.0.1:8080";
            # proxyWebsockets = true;
          };
        };
      };
      nixpkgs.system = "x86_64-linux";
    };
}
