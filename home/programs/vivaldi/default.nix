let
  extDir = "vivaldi/External Extensions/";

  google-meet-grid   = "kklailfgofogmmdlhgmjgenehkjoioip";
  google-translate   = "aapbdbdomjkkjkaonfhkkikfgjllcleb";
  lastpass           = "hdokiejnpimakedhajhdlcegeplioahd";
  picture-in-picture = "hkgfoiooedgoejojocmhlaklaeopbecg";
  vimium             = "dbepggeogbaibhgnhhndojpepiihcmeb";

  ext = builtins.toJSON {
    external_update_url = "https://clients2.google.com/service/update2/crx";
  };
in
{
  xdg.configFile."${extDir}${google-meet-grid}.json".text   = ext;
  xdg.configFile."${extDir}${google-translate}.json".text   = ext;
  xdg.configFile."${extDir}${lastpass}.json".text           = ext;
  xdg.configFile."${extDir}${picture-in-picture}.json".text = ext;
  xdg.configFile."${extDir}${vimium}.json".text             = ext;
}
