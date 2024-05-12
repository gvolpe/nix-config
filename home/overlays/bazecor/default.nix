self: super:

{
  #bazecor = super.applyPatches {
  #src = super.bazecor;
  #postPatch = ''
  #rm lib/udev/rules.d/*-10-dygma.rules
  #ln -s --target-directory=lib/udev/rules.d ${./60-dygma.rules}
  #'';
  #};

  bazecor = super.appimageTools.wrapAppImage rec {
    pname = "bazecor";
    version = "1.3.11";

    src = super.appimageTools.extract {
      inherit pname version;
      src = super.fetchurl {
        url = "https://github.com/Dygmalab/Bazecor/releases/download/v${version}/Bazecor-${version}-x64.AppImage";
        hash = "sha256-iMurQDF0CBMnJnjmEgNIKYd8C5B4FguMi4Jqa3dHr3o=";
      };

      # Workaround for https://github.com/Dygmalab/Bazecor/issues/370
      postExtract = ''
        substituteInPlace \
          $out/usr/lib/bazecor/resources/app/.webpack/main/index.js \
          --replace-fail \
            'checkUdev=()=>{try{if(c.default.existsSync(f))return c.default.readFileSync(f,"utf-8").trim()===l.trim()}catch(e){console.error(e)}return!1}' \
            'checkUdev=()=>{return 1}'
      '';
    };

    # also make sure to update the udev rules in ./10-dygma.rules; most recently
    # taken from
    # https://github.com/Dygmalab/Bazecor/blob/v1.3.11/src/main/utils/udev.ts#L6

    extraPkgs = p: (super.appimageTools.defaultFhsEnvArgs.multiPkgs p) ++ [
      p.glib
    ];

    # Also expose the udev rules here, so it can be used as:
    #   services.udev.packages = [ pkgs.bazecor ];
    # to allow non-root modifications to the keyboards.

    #ln -s $out/bin/${pname}-${version} $out/bin/${pname}
    extraInstallCommands = ''
      source "${super.makeWrapper}/nix-support/setup-hook"
      wrapProgram $out/bin/${pname} \
        --add-flags "\''${NIXOS_OZONE_WL:+\''${WAYLAND_DISPLAY:+--ozone-platform-hint=auto --enable-features=WaylandWindowDecorations}}"

      install -m 444 -D ${src}/Bazecor.desktop -t $out/share/applications
      substituteInPlace $out/share/applications/Bazecor.desktop \
        --replace 'Exec=Bazecor' 'Exec=bazecor'

      install -m 444 -D ${src}/bazecor.png -t $out/share/pixmaps

      mkdir -p $out/lib/udev/rules.d
      ln -s --target-directory=$out/lib/udev/rules.d ${./60-dygma.rules}
    '';

    meta = {
      description = "Graphical configurator for Dygma Products";
      homepage = "https://github.com/Dygmalab/Bazecor";
      changelog = "https://github.com/Dygmalab/Bazecor/releases/tag/v${version}";
      sourceProvenance = [ super.lib.sourceTypes.binaryNativeCode ];
      license = super.lib.licenses.gpl3Only;
      maintainers = with super.lib.maintainers; [ amesgen ];
      platforms = [ "x86_64-linux" ];
      mainProgram = "bazecor";
    };
  };
}
