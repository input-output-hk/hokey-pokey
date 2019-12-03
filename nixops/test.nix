{
  network.description = "hokey-pokey";

  test =
    { config, pkgs, ... }: {
      imports = [
        ./hokey-pokey.nix
      ];
      time.timeZone = "Pacific/Auckland";
      services.hokey-pokey.enable = true;
      services.openssh.allowSFTP = false;
      services.openssh.passwordAuthentication = false;
      networking.firewall.allowedTCPPorts = [ 22 80 443 ];
    };
}
