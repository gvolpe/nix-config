{ baseDir }:

let
  extDir = "${baseDir}/External Extensions/";

  x = import ./extensions.nix;

  ext = builtins.toJSON {
    external_update_url = "https://clients2.google.com/service/update2/crx";
  };
in
{
  # see brave.browser.enabled_labs_experiments for more flags
  #xdg.configFile."${baseDir}/Local State".source = ./local-state.json;

  xdg.configFile."${extDir}${x.dark-reader}.json".text       = ext;
  xdg.configFile."${extDir}${x.github-dark-theme}.json".text = ext;
  xdg.configFile."${extDir}${x.google-meet-grid}.json".text  = ext;
  xdg.configFile."${extDir}${x.google-translate}.json".text  = ext;
  xdg.configFile."${extDir}${x.lastpass}.json".text          = ext;
  xdg.configFile."${extDir}${x.nyancat-youtube}.json".text   = ext;
  xdg.configFile."${extDir}${x.vimium}.json".text            = ext;
}
