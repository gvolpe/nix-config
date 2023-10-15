self: super:

let
  # Waiting for PR: https://github.com/NixOS/nixpkgs/pull/254264
  buildBazecor =
    { lib
    , appimageTools
    , fetchurl
    , runCommand
    }:

    let
      pname = "bazecor";
      version = "1.3.4-hot-fix";

      appimageSrc = fetchurl {
        url = "https://github.com/gvolpe/bazecor-1.3.4-bug-fix/releases/download/v${version}/Bazecor-${version}-x64.AppImage";
        hash = "sha256-mi9/RiVEPEtrqEimw1Bg1QSRBM/JeqUkcB/8OkaURZk=";
        #url = "https://github.com/Dygmalab/Bazecor/releases/download/v${version}/Bazecor-${version}-x64.AppImage";
      };

      src = runCommand "${pname}-extracted"
        {
          buildInputs = [ appimageTools.appimage-exec ];
        } ''
        appimage-exec.sh -x $out ${appimageSrc}

        # Disable udev rules check to fix this annoying issue: https://github.com/Dygmalab/Bazecor/issues/370
        substituteInPlace $out/usr/lib/bazecor/resources/app/.webpack/main/index.js \
          --replace "checkUdev=()=>{try{if(c.default.existsSync(f))return c.default.readFileSync(f,\"utf-8\").trim()===l.trim()}catch(e){console.error(e)}return!1}" "checkUdev=()=>{return 1}"
      '';
    in

    appimageTools.wrapAppImage rec {
      inherit pname src version;

      extraPkgs = p: (appimageTools.defaultFhsEnvArgs.multiPkgs p) ++ [
        p.glib
      ];

      # Also expose the udev rules here, so it can be used as:
      #   services.udev.packages = [ pkgs.bazecor ];
      # to allow non-root modifications to the keyboards.

      extraInstallCommands = ''
        mv $out/bin/bazecor-* $out/bin/bazecor

        mkdir -p $out/lib/udev/rules.d
        ln -s --target-directory=$out/lib/udev/rules.d ${./60-dygma.rules}
      '';

      meta = {
        description = "Graphical configurator for Dygma Products";
        homepage = "https://github.com/Dygmalab/Bazecor";
        changelog = "https://github.com/Dygmalab/Bazecor/releases/tag/v${version}";
        sourceProvenance = [ lib.sourceTypes.binaryNativeCode ];
        license = lib.licenses.gpl3Only;
        maintainers = with lib.maintainers; [ gvolpe ];
        platforms = [ "x86_64-linux" ];
      };
    };
in
{
  bazecor = self.callPackage buildBazecor { };
}
