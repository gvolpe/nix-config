{ buildFirefoxXpiAddon, lib }:

{
  old-github-feed = buildFirefoxXpiAddon rec {
    pname = "return-old-github-feed";
    version = "1.0.1";
    addonId = "{9dfcb52a-5322-4d42-9924-9d3b8871ad90}";
    url = "https://addons.mozilla.org/firefox/downloads/file/4166201/${pname}-${version}.xpi";
    sha256 = "sha256-age3Nj2n4X3Zg1bTElChOpgDDGCQd4nOqHebW8zNsBQ=";
    meta = with lib;
      {
        homepage = "https://github.com/Siriusmart/return-github-feed";
        description = "This extension replaces the new and ugly GitHub feed with the real one from github.com/dashboard-feed";
        license = licenses.gpl3;
        platforms = platforms.all;
      };
  };
}
