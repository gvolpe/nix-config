{ stdenv, curl, jq, ripgrep, writeShellScript }:

let
  cl = "${curl}/bin/curl";
  rg = "${ripgrep}/bin/rg";
  file = "home/programs/neovim-ide/metals.nix";
  name = "metals-updater-script";

  src = writeShellScript name ''
    NEW=$(${cl} https://scalameta.org/metals/latests.json | ${jq}/bin/jq ".release" | awk -F '"' '{print $2}')
    OLD=$(${rg} "version =" ${file} | awk -F '"' '{print $2}')

    echo "Old version: $OLD"
    echo "New version: $NEW"

    # See: https://github.com/scalameta/metals/issues/5952
    if [[ $NEW != $OLD && $NEW != *"SNAPSHOT"* ]]; then
      echo "Updating metals"
      sed -i "s/$OLD/$NEW/g" ${file}

      nix build .#metals 2> build-result

      OLD_HASH=$(cat build-result | ${rg} specified: | awk -F ':' '{print $2}' | sed 's/ //g')
      NEW_HASH=$(cat build-result | ${rg} got: | awk -F ':' '{print $2}' | sed 's/ //g')

      echo "Old hash: $OLD_HASH"
      echo "New hash: $NEW_HASH"

      rm build-result

      sed -i "s|$OLD_HASH|$NEW_HASH|g" ${file}

      echo "metals_version=$NEW" >> $GITHUB_OUTPUT
    else
      echo "Versions are identical (or an unwanted snapshot), aborting."
    fi
  '';
in
stdenv.mkDerivation
{
  inherit name src;

  phases = [ "installPhase" "patchPhase" ];

  installPhase = ''
    mkdir -p $out/bin
    cp $src $out/bin/${name}
    chmod +x $out/bin/${name}
  '';
}
