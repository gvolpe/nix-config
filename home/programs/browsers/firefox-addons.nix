{ buildFirefoxXpiAddon, lib }:

{
  chatgpt = buildFirefoxXpiAddon rec {
    pname = "chatgpt_for_search_engines";
    version = "1.5.4";
    addonId = "{28c31a34-6fa8-4962-91eb-2f4777de6e68}";
    url = "https://addons.mozilla.org/firefox/downloads/file/4055458/${pname}-${version}.xpi";
    sha256 = "sha256-CDfYJN7le+F4cXWi2mK3RhhGYGzUzFX5vsHiz6Y5m+g=";
    meta = with lib;
      {
        homepage = "https://chatonai.org/";
        description = "ChatGPT for search engines";
        license = licenses.mpl20;
        platforms = platforms.all;
      };
  };
}
