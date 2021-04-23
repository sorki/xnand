{ lib, options, config, ... }:

with lib;

let

  # Takes a recursive set of options and returns all their values with a mkDefault
  takeValuesAsDefault = opts: mapAttrs (name: value:
    if isOption value
    then mkDefault value.value
    else takeValuesAsDefault value)
    (filterAttrs (name: value: ! hasPrefix "_" name) opts);

  channelOptions = {
    commands = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable the command plugin.";
      };
    };
  };

in

{
  options = {

    password = mkOption {
      type = types.str;
      description = "Password";
    };

    user = mkOption {
      type = types.str;
      description = "Name";
    };

    host = mkOption {
      type = types.str;
      description = "AMQP Host";
      default = "localhost";
    };

    port = mkOption {
      type = types.port;
      description = "AMQP Port";
      default = 5672;
    };

    stateDir = mkOption {
      type = types.path;
      description = "State dir";
      default = "/var/lib/xnand/state";
    };

    channels = mkOption {
      type = types.attrsOf (types.submodule [
        { options = channelOptions; }
        (takeValuesAsDefault options.channelDefaults)
      ]);
      default = {};
      description = "Channel-specific plugin configuration.";
    };

    channelDefaults = channelOptions;

    users = channelOptions;

    debugMode = mkOption {
      type = types.bool;
      default = false;
      description = "Enable debug mode: Only accepts messages in #bottest";
    };

  };
}
