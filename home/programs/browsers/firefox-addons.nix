{ buildFirefoxXpiAddon, lib }:

{
  chatgpt = buildFirefoxXpiAddon {
    pname = "chatgpt_for_google";
    version = "1.15.1";
    addonId = "{4b726fbc-aba9-4fa7-97fd-a42c2511ddf7}";
    url = "https://addons.mozilla.org/firefox/downloads/file/4057649/chatgpt_for_google-1.15.1.xpi";
    sha256 = "sha256-GT2X3wIxMjqPGCx9OtU0SYHMXI3vhQ1SC+KAdEHI3Q8=";
    meta = with lib;
      {
        homepage = "https://github.com/wong2/chat-gpt-google-extension";
        description = "A browser extension to display ChatGPT response alongside search engine results";
        license = licenses.gpl3;
        platforms = platforms.all;
      };
  };
}
