{ config, lib, pkgs, ... }:
with lib;
let this = import ./. { system = pkgs.system; };
    cfg = config.services.hokey-pokey;
in {
  options.services.hokey-pokey.enable = mkOption {
    type = types.bool;
    default = false;
    description = "If enabled, run hokey-pokey service";
  };
  config = mkIf cfg.enable {
    systemd.services.hokey-pokey = {
      enable = true;
      after = [
      ];
      requires = [
      ];
      serviceConfig = {
        Type = "simple";
        Restart = "always";
        RestartSec = 10;
      };
      script = ''
          ${this.hokey-pokey.components.exes.hokey-pokey}/bin/hokey-pokey
      '';
    };
  };
}

