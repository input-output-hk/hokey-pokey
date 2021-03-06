{ config, lib, pkgs, ... }:
with lib;
let this = import ./. { system = pkgs.system; };
    cfg = config.services.hokey-pokey;

    defaultUser = "hokey-pokey-agent";
    defaultUserDetails = {
      name = defaultUser;
      home = "/var/lib/hokey-pokey-agent";
      description = "System user for the hokey-pokey service";
      isSystemUser = true;
      createHome = true;
    };
in {
  options.services.hokey-pokey = {
    enable = mkOption {
      type = types.bool;
      default = false;
      description = "If enabled, run hokey-pokey service";
    };
    user = mkOption {
      description = "Unix system user that runs the hokey-pokey service";
      type = types.str;
    };
  };
  config = mkIf cfg.enable {
    services.hokey-pokey.user = mkDefault defaultUser;

    systemd.services.hokey-pokey = {
      enable = true;
      wantedBy = [ "multi-user.target" ];
      after = [ "network-online.target" ];
      serviceConfig = {
        # Type = "simple";
        Restart = "always"; # "on-failure"
        RestartSec = 10; # 120
        ExecStart = "${this.hokey-pokey-wrapped}/bin/hokey-pokey";
        User = cfg.user;
        WorkingDirectory = config.users.users.${cfg.user}.home;
        StandardOutput = "journal+console";
        StandardError = "journal+console";
      };
    };
    users = mkIf (cfg.user == defaultUser) {
      users.hokey-pokey-agent = defaultUserDetails;
    };
  };
}

