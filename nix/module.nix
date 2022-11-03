{ lib, config, pkgs, ... }:

with lib;

let

  cfg = config.services.xnand;

  filteredConfig = filterAttrsRecursive (name: value: name != "_module") cfg.config;
  configFile = pkgs.writeText "xnand-config.json" (builtins.toJSON filteredConfig);

  dataDir = "/var/lib/xnand";
in

{
  options.services.xnand = {

    enable = mkEnableOption "xnand";

    package = mkOption {
      type = types.package;
      default = pkgs.haskellPackages.xnand;
    };

    config = mkOption {
      type = types.submodule (import ./options.nix);
      default = {};
      description = "xnand configuration";
    };

    configFile = mkOption {
      type = types.path;
      description = "JSON configuration file";
    };

  };

  config = mkIf cfg.enable {

    services.xnand.configFile = mkDefault configFile;

    users.users.xnand = {
      description = "User for xnand";
      home = dataDir;
      createHome = true;
      group = "xnand";
      isSystemUser = true;
    };
    users.groups.xnand = {};

    systemd.tmpfiles.rules = [
      "d ${dataDir} 0770 xnand xnand -"
    ];

    systemd.services.xnand = {
      description = "a bot";
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];
      wants = [ ];
      path = [ ];
      unitConfig.StartLimitIntervalSec = 0;
      serviceConfig = {
        User = "xnand";
        Group = "xnand";
        ExecStart = "${cfg.package}/bin/xnand ${cfg.configFile}";
        Restart = "always";
        RestartSec = 1;
        MemoryMax = "100M";
        CPUQuota = "50%";
        WorkingDirectory = dataDir;
      };
    };

  };
}
