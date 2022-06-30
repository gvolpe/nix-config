self: super:

{
  protonvpn-gui = super.protonvpn-gui.overrideAttrs (
    old: rec {
      propagatedBuildInputs = old.propagatedBuildInputs ++ [ super.glib-networking ];
    }
  );
}

