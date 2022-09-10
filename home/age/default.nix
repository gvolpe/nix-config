let secrets = { pkgs, config, ... }: {
  homeage = {
    identityPaths = [ "~/.ssh/id_ed25519" ];
    pkg = pkgs.rage;

    file."github-token" = {
      source = ./github.age;
      path = "github-notifications-token";
      symlinks = [ "${config.xdg.configHome}/secrets/github" ];
    };
  };
};
in [ secrets ]
