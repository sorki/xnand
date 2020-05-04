{ lib, config, pkgs, ... }:

with lib;

let

  cfg = config.services.hnixbot;

  filteredConfig = filterAttrsRecursive (name: value: name != "_module") cfg.config;
  configFile = pkgs.writeText "hnixbot-config.json" (builtins.toJSON filteredConfig);

  dataDir = "/var/lib/hnixbot";
in

{
  options.services.hnixbot = {

    enable = mkEnableOption "Nixbot";

    package = mkOption {
      type = types.package;
      default = pkgs.haskellPackages.hnixbot;
    };

    config = mkOption {
      type = types.submodule (import ./nix/options.nix);
      default = {};
      description = "Nixbot configuration";
    };

    configFile = mkOption {
      type = types.path;
      description = "JSON configuration file";
    };

  };

  config = mkIf cfg.enable {

    services.hnixbot.configFile = mkDefault configFile;

    users.users.hnixbot = {
      description = "User for hnixbot";
      home = dataDir;
      createHome = true;
      group = "hnixbot";
    };
    users.groups.hnixbot = {};

    systemd.tmpfiles.rules = [
      "d ${dataDir} 0770 hnixbot hnixbot -"
    ];

    systemd.services.hnixbot = {
      description = "Nix bot";
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];
      wants = [ ];
      path = [ ];
      unitConfig.StartLimitIntervalSec = 0;
      serviceConfig = {
        User = "hnixbot";
        Group = "hnixbot";
        ExecStart = "${cfg.package}/bin/hnixbot ${cfg.configFile}";
        Restart = "always";
        RestartSec = 1;
        MemoryMax = "100M";
        CPUQuota = "50%";
        WorkingDirectory = dataDir;
      };
    };

    users.users.nginx.extraGroups = [ "hnixbot" ];

  };
}
