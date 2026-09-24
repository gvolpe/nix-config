{ lib, pkgs }:

{
  /* Return first binary executable name of the given derivation
     Type:
       exe :: Derivation -> String
  */
  exe = drv:
    let
      regFiles = lib.mapAttrsToList (f: _: f) (lib.filterAttrs (_: t: t == "regular") (builtins.readDir "${drv}/bin"));
      mainProg = drv.meta.mainProgram or (lib.head regFiles);
    in
    "${drv}/bin/${mainProg}";

  /* Return the input string with any newline marker removed.
     Type:
       removeNewline :: String -> String
  */
  removeNewline = lib.replaceStrings [ "\n" ] [ "" ];

  /* Wrap every binary from a package with extra flags and environment.

     Type:
       wrapWith :: AttrSet -> AttrSet -> Derivation

    Source licensed under EUPL: https://codeberg.org/viperML/wrapper-manager (lib.wrapWith pkgs)
  */
  mkWrapper =
    { basePackage
    , extraPackages ? [ ]
    , prependFlags ? [ ]
    , appendFlags ? [ ]
    , env ? { }
    , pathAdd ? [ ]
    , wrapperType ? "binary"
    , extraWrapperFlags ? ""
    , postBuild ? ""
    , overrideAttrs ? lib.id
    }:
    let
      packages = [ basePackage ] ++ extraPackages;

      envFlags = lib.concatLists (lib.mapAttrsToList
        (name: value:
          let
            value' = if builtins.isAttrs value then value.value or null else value;
            force = if builtins.isAttrs value then value.force or (value' == null) else false;
          in
          if value' == null
          then lib.optionals force [ "--unset" name ]
          else [
            (if force then "--set" else "--set-default")
            name
            "${value'}"
          ])
        env);

      wrapFlags =
        lib.concatMap (flag: [ "--add-flag" "${flag}" ]) prependFlags
        ++ lib.concatMap (flag: [ "--append-flag" "${flag}" ]) appendFlags
        ++ lib.optionals (pathAdd != [ ]) [ "--prefix" "PATH" ":" (lib.makeBinPath pathAdd) ]
        ++ envFlags;

      wrapProgram =
        if wrapperType == "binary" then "wrapProgramBinary"
        else if wrapperType == "shell" then "wrapProgramShell"
        else throw "Unsupported wrapperType: ${wrapperType}";

      hasManOutput = builtins.any (pkg: pkg ? man) packages;

      manLinks = lib.concatMapStringsSep "\n"
        (pkg: lib.optionalString (pkg ? man) ''
          ${lib.getExe pkgs.lndir} -silent ${pkg.man} ''${!outputMan}
        '')
        packages;
    in
    (pkgs.symlinkJoin {
      name = "${lib.getName basePackage}-${lib.getVersion basePackage}";
      paths = packages;
      nativeBuildInputs = [
        pkgs.makeBinaryWrapper
        pkgs.makeWrapper
      ];
      outputs = [ "out" ] ++ lib.optional hasManOutput "man";
      passthru = (basePackage.passthru or { }) // {
        unwrapped = basePackage;
      };
      meta = (basePackage.meta or { }) // {
        outputsToInstall = [ "out" ] ++ lib.optional hasManOutput "man";
      };
      postBuild = ''
        shopt -s nullglob

        for file in "$out/bin/"*; do
          if [[ -f "$file" && -x "$file" ]]; then
            :
            ${lib.optionalString (wrapFlags != [ ] || extraWrapperFlags != "") ''
              ${wrapProgram} "$file" ${lib.escapeShellArgs wrapFlags} ${extraWrapperFlags}
            ''}
          fi
        done

        ${lib.optionalString hasManOutput ''
          mkdir -p ''${!outputMan}
          ${manLinks}
        ''}

        ${postBuild}
      '';
    }).overrideAttrs overrideAttrs;

  /* Return either `emptyValue` or `fileAction filepath`. Both need to be of the same type.

     Example:
       token = lib.secretManager {
         filepath = ./secrets/access-token;
         fileAction = file: lib.removeNewline (lib.readFile file);
         encryptedSha256 = lib.fakeSha256;
         emptyValue = "SECRET";
       };

     Type:
       secretManager :: String -> (String -> a) -> String -> a -> a
  */
  secretManager =
    { emptyValue
    , encryptedSha256
    , fileAction ? import
    , filepath
    }:
    let
      fileHash = builtins.hashFile "sha256" filepath;
    in
    if fileHash == encryptedSha256 then emptyValue else (fileAction filepath);
}
