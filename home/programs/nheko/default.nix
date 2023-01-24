{ pkgs, lib, specialArgs, ... }:

let
  token = lib.secretManager {
    filepath = ../../secrets/nheko-access-token;
    fileAction = file: lib.removeNewline (builtins.readFile file);
    encryptedSha256 = "37c4e7f9867f1005ce6247a38fa142a5a3682d755d3c57de531e9e23a0d05be7";
    emptyValue = "SECRET";
  };
in
{
  programs.nheko = {
    enable = true;

    settings = {
      general = {
        disableCertificateValidation = false;
      };

      auth = {
        accessToken = token;
        deviceId = "CGDVEGHPLX";
        homeServer = "https://matrix-client.matrix.org:443";
        userId = "@@gvolpe:matrix.org";
      };

      sidebar = {
        width = 416;
      };

      settings = {
        scaleFactor = if specialArgs.hidpi then 1.0 else 0.7;
      };

      user = {
        alertOnNotification = true;
        animateImagesOnHover = false;
        automaticallyShareKeysWithTrustedUsers = false;
        avatarCircles = true;
        bubblesEnabled = false;
        decryptSidebar = true;
        desktopNotifications = true;
        emojiFontFamily = "Noto Emoji";
        exposeDbusApi = false;
        fontFamily = "JetBrainsMonoMedium Nerd Font Mono";
        fontSize = 9;
        groupView = true;
        markdownEnabled = true;
        minorEvents = false;
        mobileMode = false;
        mutedTags = "global";
        onlineKeyBackup = false;
        onlyShareKeysWithVerifiedUsers = false;
        openImageExternal = false;
        openVideoExternal = false;
        presence = "AutomaticPresence";
        privacyScreen = false;
        privacyScreenTimeout = 0;
        readReceipts = true;
        ringtone = "Mute";
        shareKeysWithTrustedUsers = true;
        smallAvatarsEnabled = false;
        "sidebar\\communityListWidth" = 40;
        "sidebar\\roomListWidth" = 308;
        sortByUnread = true;
        spaceNotifications = true;
        theme = "dark";
        "timeline\\buttons" = true;
        "timeline\\enlargeEmojiOnlyMsg" = true;
        "timeline\\maxWidth" = 0;
        "timeline\\messageHoverHighlight" = false;
        typingNotifications = true;
        useIdenticon = true;
        useStunServer = false;
        "window\\startInTray" = false;
        "window\\tray" = true;
      };

      window = {
        height = 482;
        width = 950;
      };

    };
  };
}
