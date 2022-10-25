self: super:

{
  nautilus-patched = super.gnome.nautilus.overrideAttrs (old: {
    patches = [
      # Switch to GTK 4 settings schema to avoid crash when GTK 3 did not manage to contaminate environment.
      # https://gitlab.gnome.org/GNOME/nautilus/-/merge_requests/1013
      (super.fetchpatch {
        url = "https://gitlab.gnome.org/GNOME/nautilus/-/commit/96d542a0d84da4ad6915a7727642490a5c433d4a.patch";
        sha256 = "BO/0ifRwSTDe7RV+DI3CPZg+UQezk0tbM+UidgoJRQM=";
      })
      (super.fetchpatch {
        url = "https://gitlab.gnome.org/GNOME/nautilus/-/commit/52b4daf4396fd3b21755b3a0d1fbf85c3831c6b1.patch";
        sha256 = "+8KCw2HZUi9UgOEUBNp4kbwqOI1qz6i0Q/wvzqTb8OA=";
      })
    ] ++ old.patches;
  });
}
