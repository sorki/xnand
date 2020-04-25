{ lib, config, pkgs, ... }:

with lib;

let

  cfg = config.services.hnixbot;

  filteredConfig = filterAttrsRecursive (name: value: name != "_module") cfg.config;
  configFile = pkgs.writeText "hnixbot-config.json" (builtins.toJSON filteredConfig);

  hnixbot = import ./default.nix;
in

{
  options.services.hnixbot = {

    enable = mkEnableOption "Nixbot";

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
      home = "/var/lib/hnixbot";
      createHome = true;
      group = "hnixbot";
    };
    users.groups.hnixbot = {};

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
        ExecStart = "${hnixbot}/bin/hnixbot ${cfg.configFile}";
        Restart = "always";
        RestartSec = 1;
        MemoryMax = "100M";
        CPUQuota = "50%";
        WorkingDirectory = "/var/lib/hnixbot/state/nixpkgs";
      };
    };

    users.users.nginx.extraGroups = [ "hnixbot" ];

  };
}
