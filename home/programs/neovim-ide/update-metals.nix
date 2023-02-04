{ system, curl, jq, ripgrep, writeShellScript }:

let
  cl = "${curl}/bin/curl";
  rg = "${ripgrep}/bin/rg";
  file = "home/programs/neovim-ide/metals.nix";
in
writeShellScript "update-metals" ''
  NEW=$(${cl} https://scalameta.org/metals/latests.json | ${jq}/bin/jq ".snapshot" | awk -F '"' '{print $2}')
  OLD=$(${rg} "version =" ${file} | awk -F '"' '{print $2}')

  echo "Old version: $OLD"
  echo "New version: $NEW"

  if [ $NEW != $OLD ]; then
    echo "Updating metals"
    sed -i "s/$OLD/$NEW/g" ${file}

    nix build .#packages.${system}.metals 2> build-result

    OLD_HASH=$(cat build-result | ${rg} specified: | awk -F ':' '{print $2}' | sed 's/ //g')
    NEW_HASH=$(cat build-result | ${rg} got: | awk -F ':' '{print $2}' | sed 's/ //g')

    echo "Old hash: $OLD_HASH"
    echo "New hash: $NEW_HASH"

    rm build-result

    sed -i "s|$OLD_HASH|$NEW_HASH|g" ${file}

    echo "metals_version=$NEW" >> $GITHUB_OUTPUT
  else
    echo "Versions are identical, aborting."
  fi
''
