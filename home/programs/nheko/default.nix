{ pkgs, lib, specialArgs, ... }:

let
  removeNewline = lib.replaceStrings [ "\n" ] [ "" ];
  tokenFile = ../../secrets/nheko-access-token;
  fileHash = builtins.hashFile "sha256" tokenFile;
  encryptedHash = "856276f8af9309f3e8c70d986fa77250d9b95602dc1d44f32d2807bb5bc634b6";
  token = removeNewline (if fileHash == encryptedHash then "SECRET" else (builtins.readFile tokenFile));
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
        deviceId = "DWLAOURPHA";
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
