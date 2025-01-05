{ lib, ... }:

with lib;

{
  options = {
    secrets = {
      githubToken = mkOption {
        type = types.str;
        default = "OVERRIDE_ME";
      };
      ngrokToken = mkOption {
        type = types.str;
        default = "OVERRIDE_ME";
      };
      openaiApiKey = mkOption {
        type = types.str;
        default = "OVERRIDE_ME";
      };
    };
  };
}
