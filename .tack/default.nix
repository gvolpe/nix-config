# SPDX-License-Identifier: EUPL-1.2
# tack-managed resolver. delete this line to take ownership; tack will leave it alone afterwards.
# tack-resolver: patched patchedDiff tag version signedBy groups archive defaultType nestedGroups topFollows public

let
  inherit (builtins)
    addErrorContext
    all
    attrNames
    attrValues
    compareVersions
    concatMap
    concatStringsSep
    any
    elem
    elemAt
    fetchGit
    filter
    foldl'
    fromJSON
    genericClosure
    hashFile
    head
    intersectAttrs
    isAttrs
    isList
    isString
    length
    listToAttrs
    mapAttrs
    match
    nixVersion
    path
    pathExists
    readFile
    readFileType
    removeAttrs
    replaceStrings
    split
    stringLength
    substring
    tail
    trace
    warn
    ;

  validateInputNames =
    { value, location }:
    if !isList value || any (name: !isString name) value then
      throw "tack: ${location} must be an array of strings"
    else
      value;

  isPinGroup =
    entry:
    isAttrs entry
    && !(entry ? url)
    && all isAttrs (attrValues entry)
    && any (pin: pin ? url) (attrValues entry);

  flattenInputs =
    inputs:
    foldl' (
      acc: name:
      let
        entry = inputs.${name};
        pins =
          if isPinGroup entry then
            mapAttrs (
              pin: value:
              if value ? group then
                throw "tack: input '${pin}' sits under [inputs.${name}], so it can't also set group"
              else
                value // { group = name; }
            ) entry
          else
            { ${name} = entry; };
        clashes = filter (pin: acc ? ${pin}) (attrNames pins);
      in
      if clashes != [ ] then throw "tack: input '${head clashes}' is defined twice" else acc // pins
    ) { } (attrNames inputs);

  pinsOf =
    toml:
    mapAttrs (_: pin: if pin ? group then (toml.groups or { }).${pin.group} or { } // pin else pin) (
      flattenInputs (toml.inputs or { })
    );

  knownTypes = [
    "github"
    "gitlab"
    "git"
    "tarball"
    "path"
    "indirect"
  ];

  # fetchTree's lastModifiedDate, YYYYMMDDHHMMSS in UTC
  formatEpoch =
    secs:
    let
      days = secs / 86400;
      daySecs = secs - days * 86400;
      shifted = days + 719468;
      era = shifted / 146097;
      dayOfEra = shifted - era * 146097;
      yearOfEra = (dayOfEra - dayOfEra / 1460 + dayOfEra / 36524 - dayOfEra / 146096) / 365;
      dayOfYear = dayOfEra - (365 * yearOfEra + yearOfEra / 4 - yearOfEra / 100);
      marchMonth = (5 * dayOfYear + 2) / 153;
      day = dayOfYear - (153 * marchMonth + 2) / 5 + 1;
      month = if marchMonth < 10 then marchMonth + 3 else marchMonth - 9;
      year = yearOfEra + era * 400 + (if month <= 2 then 1 else 0);
      pad = n: if n < 10 then "0${toString n}" else toString n;
    in
    "${toString year}${pad month}${pad day}${pad (daySecs / 3600)}${
      pad (daySecs / 60 - daySecs / 3600 * 60)
    }${pad (daySecs - daySecs / 60 * 60)}";

  fetchTreeAttrs = {
    type = null;
    owner = null;
    repo = null;
    host = null;
    url = null;
    id = null;
    ref = null;
    rev = null;
    narHash = null;
    lastModified = null;
    revCount = null;
    submodules = null;
    shallow = null;
    allRefs = null;
    name = null;
    lfs = null;
    exportIgnore = null;
    verifyCommit = null;
    keytype = null;
    publicKey = null;
    publicKeys = null;
    dirtyRev = null;
    dirtyShortRev = null;
    unpack = null;
    treeHash = null;
  };

  fetchGitAttrs = {
    url = null;
    ref = null;
    rev = null;
    narHash = null;
    submodules = null;
    shallow = null;
    allRefs = null;
    lfs = null;
    verifyCommit = null;
    keytype = null;
    publicKey = null;
    publicKeys = null;
  };

  # fetchTarball ignores access-tokens, so prefer fetchTree when available, except
  # for the public pins tack update marks
  fetchLocked =
    {
      what,
      locked,
      public ? false,
    }:
    let
      type = locked.type or "";
      host = default: locked.host or default;
      verifyCommit = locked.verifyCommit or (locked ? publicKey || locked ? publicKeys);
      archiveUrl = {
        github = "https://${host "github.com"}/${locked.owner}/${locked.repo}/archive/${locked.rev}.tar.gz";
        gitlab = "https://${host "gitlab.com"}/api/v4/projects/${locked.owner}%2F${locked.repo}/repository/archive.tar.gz?sha=${locked.rev}";
        sourcehut = "https://${host "git.sr.ht"}/${locked.owner}/${locked.repo}/archive/${locked.rev}.tar.gz";
      };
      revInfo =
        if locked ? rev then
          {
            inherit (locked) rev;
            shortRev = substring 0 7 locked.rev;
          }
        else
          { };
      dateInfo =
        if locked ? lastModified then
          {
            inherit (locked) lastModified;
            lastModifiedDate = formatEpoch locked.lastModified;
          }
        else
          { };
      tarball = url: {
        outPath = fetchTarball {
          inherit url;
          sha256 = locked.narHash;
        };
        inherit (locked) narHash;
      };
    in
    if
      builtins ? fetchTree && !(public && archiveUrl ? ${type} && locked ? rev && locked ? narHash)
    then
      builtins.fetchTree locked
    else if !(locked ? narHash) then
      throw "tack: ${what} has no narHash and requires builtins.fetchTree"
    else if archiveUrl ? ${type} && locked ? rev then
      tarball archiveUrl.${type} // revInfo // dateInfo
    else if type == "tarball" then
      tarball locked.url // revInfo // dateInfo
    else if type == "git" && locked ? rev then
      dateInfo
      // {
        submodules = locked.submodules or false;
      }
      // removeAttrs (fetchGit (
        removeAttrs (intersectAttrs fetchGitAttrs locked) (
          (if locked.lfs or false then [ ] else [ "lfs" ])
          ++ (
            if verifyCommit then
              [ ]
            else
              [
                "verifyCommit"
                "keytype"
                "publicKey"
                "publicKeys"
              ]
          )
        )
        # nix 2.20 added exportIgnore and enabled it by default, and lix and older nix reject
        # it, lfs and verifyCommit outright. lix reports 2.18.3-lix
        // (
          if compareVersions nixVersion "2.20" >= 0 then
            { exportIgnore = locked.exportIgnore or false; }
          else
            { }
        )
        // (if locked.allRefs or false then { } else { shallow = locked.shallow or true; })
      )) [ "revCount" ]
      // (if locked ? revCount then { inherit (locked) revCount; } else { })
    else if type == "path" && substring 0 1 locked.path == "/" then
      {
        outPath = path {
          inherit (locked) path;
          sha256 = locked.narHash;
          name = "source";
        };
        inherit (locked) narHash;
        lastModified = locked.lastModified or 0;
        lastModifiedDate = formatEpoch (locked.lastModified or 0);
      }
    else
      throw "tack: ${what} has type '${type}', which requires builtins.fetchTree";

  call =
    {
      overrides ? { },
      resolverDir ? ./.,
      pkgs ? null,
      system ? builtins.currentSystem or null,
      fetch_mode ? null,
    }:
    let
      pins = fromTOML (readFile (resolverDir + "/pins.toml"));
      lock = fromJSON (readFile (resolverDir + "/pins.lock.json"));
      declared =
        if
          any (reserved: pins.inputs or { } ? ${reserved}) [
            "_meta"
            "_patched"
            "_nested"
          ]
        then
          throw "tack: '_meta', '_patched' and '_nested' are reserved by tack, rename that input"
        else
          removeAttrs allPins (attrNames topFollowsRaw);
      allPins = pinsOf pins;
      topFollowsRaw = listToAttrs (
        concatMap (
          name:
          let
            pin = allPins.${name};
          in
          if !isString (pin.follows or null) then
            [ ]
          else if attrNames pin != [ "follows" ] then
            throw "tack: input '${name}' follows '${pin.follows}', so it can't set any other key or sit in a group"
          else
            [
              {
                inherit name;
                value = pin.follows;
              }
            ]
        ) (attrNames allPins)
      );
      flattenTarget =
        seen: target:
        let
          path = filter isString (split "/" target);
          first = head path;
        in
        if !isString target || !(topFollowsRaw ? ${first}) then
          target
        else if elem first seen then
          throw "tack: top-level follows loop through ${concatStringsSep " -> " (seen ++ [ first ])}"
        else
          flattenTarget (seen ++ [ first ]) (concatStringsSep "/" ([ topFollowsRaw.${first} ] ++ tail path));
      topFollows = mapAttrs (name: flattenTarget [ name ]) topFollowsRaw;
      all_follow_raw = pins.all_follow or { };
      # tackOverrides carries this reserved entry across nested resolver boundaries
      inheritedPolicy = overrides.__tack_policy or { };
      inheritedFollows = inheritedPolicy.follows or { };
      inheritedFollowMeta = inheritedPolicy.followMeta or { };
      inheritedOverrideMeta = inheritedPolicy.meta or { };
      inheritedOmitted = inheritedPolicy.omitted or [ ];
      inheritedOmitLayers = (inheritedPolicy.omit or { }).layers or [ ];
      pinOverrides = removeAttrs overrides [ "__tack_policy" ];
      forcedMode =
        let
          value = if fetch_mode != null then fetch_mode else inheritedPolicy.fetchMode or null;
        in
        if
          elem value [
            null
            "build"
            "eval"
          ]
        then
          value
        else
          throw "tack: fetch_mode must be 'build' or 'eval', got '${toString value}'";
      pinFetchMode =
        pin:
        if forcedMode != null then
          forcedMode
        else
          pin.fetch_mode or (pins.tack or { }).fetch_mode or "eval";

      all_omit_inputs =
        if !(pins ? omit_inputs) then
          [ ]
        else if !isAttrs pins.omit_inputs then
          throw "tack: omit_inputs must be a table with a names array"
        else
          validateInputNames {
            value = pins.omit_inputs.names or [ ];
            location = "omit_inputs.names";
          };

      checkFollowKeys =
        { follows, location }:
        let
          scopedName = key: match "(flake|tack):(.*)" key;
          clashes = filter (key: scopedName key != null && follows ? ${elemAt (scopedName key) 1}) (
            attrNames follows
          );
          clash = head clashes;
        in
        if clashes == [ ] then
          follows
        else
          throw "tack: ${location} has both '${elemAt (scopedName clash) 1}' and '${clash}', keep only one";

      # flatten `target = [aliases]` rows alongside `alias = "target"` rows
      all_follow = listToAttrs (
        concatMap (
          key:
          let
            val = all_follow_raw.${key};
          in
          if isList val then
            [
              {
                name = key;
                value = key;
              }
            ]
            ++ map (a: {
              name = a;
              value = key;
            }) val
          else if isString val then
            [
              {
                name = key;
                value = val;
              }
            ]
          else
            [ ]
        ) (attrNames all_follow_raw)
      );

      # path nodes are convenience pins, so return the live local path directly
      # because fetchTree rejects unlocked paths in pure eval
      fetchNode =
        {
          dir,
          name,
          node,
        }:
        let
          type = node.type or "";
        in
        if type == "path" then
          {
            outPath = if substring 0 1 node.path == "/" then node.path else dir + ("/" + node.path);
            lastModified = node.lastModified or 0;
          }
          // (if node ? narHash then { inherit (node) narHash; } else { })
        # a cold fetcher cache makes fetchTree re-unpack a locked tarball into the
        # git cache even when its store path exists, fetchTarball checks the store first
        else if type == "tarball" && node ? narHash && node ? lastModified then
          {
            outPath = fetchTarball {
              inherit (node) url;
              sha256 = node.narHash;
            };
            inherit (node) narHash lastModified;
            lastModifiedDate = formatEpoch node.lastModified;
          }
          // (
            if node ? rev then
              {
                inherit (node) rev;
                shortRev = substring 0 7 node.rev;
              }
            else
              { }
          )
        else if !(elem type knownTypes) then
          throw "tack: unknown lock type '${type}' for pin '${name}'"
        else
          # nix's fetchTree downloads a rev again even when tack update added it to
          # the store, fetchTarball finds it there
          fetchLocked {
            what = "pin '${name}'";
            locked = intersectAttrs fetchTreeAttrs node;
            public = node.public or false;
          };

      fetchPin =
        name:
        if !(lock ? ${name}) then
          throw "tack: pin '${name}' has no lock entry; run tack update"
        else
          fetchNode {
            dir = resolverDir;
            inherit name;
            node = lock.${name};
          };

      fetchFixed =
        { name, entry }:
        let
          # tack update adds the file to the store under this name, see fetch_fixed_pin
          raw = derivation {
            inherit name;
            inherit (entry) url;
            builder = "builtin:fetchurl";
            system = "builtin";
            outputHash = entry.sha256;
            outputHashAlgo = "sha256";
            outputHashMode = "flat";
          };
          unpacked = derivation {
            inherit name;
            builder = "builtin:unpack-channel";
            system = "builtin";
            src = raw;
            channelName = name;
          };
        in
        if (entry.unpack or "file") == "tarball" then unpacked.outPath + "/" + name else raw.outPath;

      # lix's builtin:fetchurl requires outputHashAlgo
      fetchArchive =
        node:
        let
          raw = derivation {
            name = "source-archive";
            url =
              {
                github = "https://codeload.github.com/${node.owner}/${node.repo}/tar.gz/${node.rev}";
                tarball = node.url;
              }
              .${node.type};
            builder = "builtin:fetchurl";
            system = "builtin";
            outputHash = node.archive.hash;
            outputHashAlgo = "sha256";
            outputHashMode = "flat";
            preferLocalBuild = true;
            impureEnvVars = [
              "http_proxy"
              "https_proxy"
              "ftp_proxy"
              "all_proxy"
              "no_proxy"
            ];
          };
          unpacked = derivation {
            name = "source";
            builder = "builtin:unpack-channel";
            system = "builtin";
            src = raw;
            channelName = "source";
            outputHash = node.archive.outHash;
            outputHashAlgo = "sha256";
            outputHashMode = "recursive";
            preferLocalBuild = true;
          };
        in
        unpacked.outPath + "/source";

      patchedTree =
        { name, pin }:
        let
          tree =
            lock.${name}.patched
              or (throw "tack: pin '${name}' has patches but no patched tree, run tack update ${name}");
          vendored =
            digest:
            let
              file = resolverDir + "/${digest.file}";
            in
            if pathExists file then
              file
            else
              throw "tack: patch ${digest.file} for pin '${name}' is missing, if this is a flake make sure it is tracked by git (git add .tack/patches)";
          current =
            map (digest: digest.source) tree.patches == pin.patches
            && all (digest: hashFile "sha256" (vendored digest) == digest.sha256) tree.patches;
        in
        if current then
          tree
        else
          throw "tack: patches for '${name}' changed since the lock was written, run tack update ${name}";

      # tack builds patched trees and adds them to the store, so eval only
      # fetches a locked path and never builds
      fetchPatched =
        { name, pin }:
        let
          node = lock.${name} or { };
          tree = patchedTree { inherit name pin; };
          fetched =
            addErrorContext
              "tack: the patched tree of '${name}' is not in the store, run tack materialize or nix-build .tack -A _patched -o .tack/patched, or tack update ${name} if the lock was edited by hand"
              (fetchLocked {
                what = "the patched tree of '${name}'";
                locked = {
                  type = "path";
                  inherit (tree) path narHash;
                }
                // (if tree ? lastModified then { inherit (tree) lastModified; } else { });
              });
        in
        fetched
        // (
          if node ? rev then
            {
              dirtyRev = node.rev + "-dirty";
              dirtyShortRev = substring 0 7 node.rev + "-dirty";
            }
          else
            { }
        );

      patchTools =
        if pkgs != null then
          pkgs
        else if !(lock ? nixpkgs) then
          throw "tack: building patched trees needs a nixpkgs pin, or pass one with nix-build .tack -A _patched --arg pkgs 'import <nixpkgs> {}'"
        else if system == null then
          throw "tack: building patched trees needs a system argument in pure eval"
        else
          import (fetchPin "nixpkgs") {
            inherit system;
            config = { };
            overlays = [ ];
          };

      ownPatched = listToAttrs (
        map (name: {
          inherit name;
          value = buildPatched {
            inherit name;
            pin = declared.${name};
          };
        }) (filter (name: (declared.${name}.patches or [ ]) != [ ]) (attrNames declared))
      );

      # pinned projects are read as data, never imported, since their resolvers
      # are upstream code
      plainName = name: isString name && match "[A-Za-z0-9_+-][A-Za-z0-9._+-]*" name != null;
      plainDir =
        dir:
        isString dir
        && match "[A-Za-z0-9_+-][A-Za-z0-9._+-]*(/[A-Za-z0-9_+-][A-Za-z0-9._+-]*)*" dir != null;
      sriHash = hash: isString hash && match "sha256-[A-Za-z0-9+/]{43}=" hash != null;
      fetchedNode =
        node:
        isAttrs node
        && sriHash (node.narHash or null)
        && elem (node.type or "") [
          "github"
          "gitlab"
          "git"
          "tarball"
        ];
      # readFileType throws below a symlink
      isType =
        type: root: relative:
        (foldl'
          (
            parent: part:
            let
              path = parent.path + "/${part}";
            in
            {
              inherit path;
              type = if parent.type == "directory" && pathExists path then readFileType path else null;
            }
          )
          {
            path = root;
            type = "directory";
          }
          (filter (part: isString part && part != "") (split "/" relative))
        ).type == type;

      tackProjectAt =
        root:
        let
          dir = root + "/.tack";
          pinsFile = fromTOML (readFile (dir + "/pins.toml"));
          lockFile = fromJSON (readFile (dir + "/pins.lock.json"));
          inputs = if isAttrs (pinsFile.inputs or null) then pinsOf pinsFile else { };
        in
        if !isType "regular" root ".tack/pins.toml" || !isType "regular" root ".tack/pins.lock.json" then
          null
        else
          {
            inherit dir inputs;
            lock = if isAttrs lockFile then intersectAttrs inputs lockFile else { };
          };

      lockedPatch =
        project: name:
        let
          node = project.lock.${name};
          tree = node.patched;
          diff = project.dir + "/diffs/${name}.diff";
        in
        if !plainName name || !isAttrs (node.patched or null) then
          { skip = null; }
        else if !fetchedNode node || !sriHash (tree.narHash or null) then
          { skip = "its lock entry isn't a locked fetch"; }
        else if !isString (tree.diff or null) then
          { skip = "its lock predates generated diffs"; }
        else if
          !isType "regular" project.dir "diffs/${name}.diff" || hashFile "sha256" diff != tree.diff
        then
          { skip = "diffs/${name}.diff is missing or doesn't match its lock"; }
        else
          {
            inherit diff;
            inherit (tree) narHash;
            drv = patchDerivation {
              upstream = fetchNode {
                inherit (project) dir;
                inherit name node;
              };
              inherit diff;
              inherit (tree) narHash;
            };
          };

      pinnedProjects =
        project:
        concatMap (
          name:
          let
            pin = project.inputs.${name};
            node = project.lock.${name} or null;
            subdir = if pin ? dir then "/" + pin.dir else "";
            patch = lockedPatch project name;
            tackHeader = "\ndiff --git \"?a/${
              replaceStrings [ "." "+" ] [ "\\." "\\+" ] (if pin ? dir then pin.dir + "/" else "")
            }\\.tack/";
            patchesTack = patch ? drv && length (split tackHeader ("\n" + readFile patch.diff)) > 1;
            source =
              (fetchNode {
                inherit (project) dir;
                inherit name node;
              }).outPath;
            root = source + subdir;
          in
          if
            !plainName name
            || !isAttrs pin
            || (pin.type or "") == "fixed"
            || pin ? dir && !plainDir pin.dir
            || !fetchedNode node
          then
            [ ]
          else
            [
              {
                key = node.narHash + subdir + (if patchesTack then "#patched" else "");
                project =
                  if patchesTack then
                    trace "tack: not following the pins of '${name}' in ${project.dir}, its patch rewrites its .tack" null
                  else if isType "directory" source subdir then
                    tackProjectAt root
                  else
                    null;
              }
            ]
        ) (attrNames project.inputs);

      nestedProjects = genericClosure {
        startSet = pinnedProjects {
          dir = resolverDir;
          inputs = declared;
          inherit lock;
        };
        operator = item: if item.project == null then [ ] else pinnedProjects item.project;
      };

      nestedPatched = listToAttrs (
        concatMap (
          item:
          let
            inherit (item) project;
            build =
              name:
              let
                patch = lockedPatch project name;
              in
              if patch ? drv then
                [
                  {
                    name = "${name}-${replaceStrings [ "/" ] [ "_" ] (substring 7 12 patch.narHash)}";
                    value = patch.drv;
                  }
                ]
              else if patch.skip == null then
                [ ]
              else
                trace "tack: skipping patched pin '${name}' of ${project.dir}, ${patch.skip}" [ ];
          in
          if project == null then [ ] else concatMap build (attrNames project.lock)
        ) nestedProjects
      );

      buildPatched =
        { name, pin }:
        let
          tree = patchedTree { inherit name pin; };
          diff = resolverDir + "/diffs/${name}.diff";
        in
        patchDerivation {
          upstream = fetchPin name;
          diff =
            if !(tree ? diff) then
              throw "tack: the lock has no generated diff for '${name}', run tack update ${name}"
            else if !pathExists diff then
              throw "tack: diffs/${name}.diff is missing, if this is a flake make sure it is tracked by git (git add .tack/diffs)"
            else if hashFile "sha256" diff != tree.diff then
              throw "tack: diffs/${name}.diff doesn't match the lock, run tack update ${name}"
            else
              diff;
          inherit (tree) narHash;
        };

      # same name and hash as tack's tree, so the same store path
      patchDerivation =
        {
          upstream,
          diff,
          narHash,
        }:
        derivation {
          name = "source";
          inherit (patchTools.stdenv.buildPlatform) system;
          builder = "${patchTools.bash}/bin/bash";
          args = [
            "-euc"
            ''
              cp -R "$upstream" "$out"
              chmod -R u+w "$out"
              cd "$out"
              if [ -s "$diff" ]; then git apply "$diff"; fi
            ''
          ];
          PATH = concatStringsSep ":" (
            map (tool: "${tool}/bin") [
              patchTools.coreutils
              patchTools.gitMinimal
            ]
          );
          HOME = "/homeless-shelter";
          upstream = upstream.outPath;
          inherit diff;
          outputHash = narHash;
          outputHashMode = "recursive";
          outputHashAlgo = "sha256";
        };

      metaFields = {
        github = [
          "owner"
          "repo"
          "host"
          "tag"
          "version"
          "rev"
          "narHash"
          "lastModified"
        ];
        gitlab = [
          "owner"
          "repo"
          "host"
          "tag"
          "version"
          "rev"
          "narHash"
          "lastModified"
        ];
        git = [
          "url"
          "ref"
          "tag"
          "version"
          "rev"
          "narHash"
          "lastModified"
        ];
        tarball = [
          "url"
          "rev"
          "narHash"
          "lastModified"
        ];
        indirect = [ "id" ];
        path = [
          "path"
          "narHash"
          "lastModified"
        ];
        fixed = [
          "url"
          "tag"
          "version"
          "sha256"
          "unpack"
        ];
      };
      metaDefaults = {
        github.host = "github.com";
        gitlab.host = "gitlab.com";
        fixed.unpack = "file";
      };
      lockedFields = {
        rev = null;
        narHash = null;
        lastModified = null;
      };

      # indirect entries lock only an id, so their revision comes from the registry fetch
      pinMeta =
        name:
        let
          entry = lock.${name} or (throw "tack: pin '${name}' has no lock entry; run tack update");
          type = entry.type or "unknown";
          fields = listToAttrs (
            map (field: {
              name = field;
              value = null;
            }) (metaFields.${type} or [ ])
          );
          fetched = if type == "indirect" then intersectAttrs lockedFields (fetchPin name) else { };
        in
        (metaDefaults.${type} or { })
        // intersectAttrs fields entry
        // fetched
        // intersectAttrs { group = null; } (declared.${name} or { })
        // {
          inherit type;
        };

      resolveFollowMeta = mapAttrs (_: target: selfMeta.${target} or { type = "upstream"; });

      resolveSpec =
        { upLock, spec }:
        if isList spec then
          walkPath {
            inherit upLock;
            nodeName = upLock.root;
            path = spec;
          }
        else
          spec;

      walkPath =
        {
          upLock,
          nodeName,
          path,
        }:
        if path == [ ] then
          nodeName
        else if !(upLock.nodes ? ${nodeName}) then
          throw "tack: follows path dead-end: no node '${nodeName}' in flake.lock"
        else
          let
            key = head path;
            inputs = upLock.nodes.${nodeName}.inputs or { };
          in
          if !(inputs ? ${key}) then
            throw "tack: follows path dead-end: node '${nodeName}' has no input '${key}'"
          else
            walkPath {
              inherit upLock;
              nodeName = resolveSpec {
                inherit upLock;
                spec = inputs.${key};
              };
              path = tail path;
            };

      followsFor =
        { name, pin }:
        let
          # a rule into this pin's own inputs would make that input follow itself
          prefix = name + "/";
          intoSelf = filter (
            k: substring 0 (stringLength prefix) (flattenTarget [ ] all_follow.${k}) == prefix
          ) (attrNames all_follow);
          local = pin.follows or { };
          sameEdge = filter (
            k:
            let
              m = match "(flake|tack):(.*)" k;
            in
            flattenTarget [ ] local.${k} == prefix + (if m == null then k else elemAt m 1)
          ) (if isAttrs local then attrNames local else [ ]);
        in
        {
          level = {
            global = removeAttrs all_follow intoSelf;
            # other pins follow these, so an omit must not throw them away
            selfFollowed = map (
              key:
              let
                m = match "(flake|tack):(.*)" key;
              in
              if m == null then key else elemAt m 1
            ) intoSelf;
            local = if isAttrs local then removeAttrs local sameEdge else local;
            localLocation = "inputs.${name}.follows";
            excluded = pin.exclude_follow or [ ];
            inherited = inheritedFollows;
            inheritedMeta = inheritedFollowMeta;
          };
          deep = {
            global = all_follow;
            local = { };
            localLocation = "inputs.${name}.follows";
            excluded = pin.exclude_follow or [ ];
            inherited = inheritedFollows;
            inheritedMeta = inheritedFollowMeta;
          };
        };

      matchesInput =
        {
          rules,
          side,
          name,
        }:
        any (rule: rule == "*" || rule == name || rule == "${side}:*" || rule == "${side}:${name}") rules;

      omitsFor =
        { name, pin }:
        let
          localOmitted = validateInputNames {
            value = pin.omit_inputs or [ ];
            location = "inputs.${name}.omit_inputs";
          };
          localKept = validateInputNames {
            value = pin.keep_inputs or [ ];
            location = "inputs.${name}.keep_inputs";
          };
        in
        builtins.deepSeq localOmitted (
          builtins.deepSeq localKept {
            layers = inheritedOmitLayers ++ [
              {
                omitted = all_omit_inputs ++ localOmitted;
                kept = localKept;
              }
            ];
          }
        );

      # layers run outermost first and a keep only cancels omits from its own
      # layer or deeper, so a nested project cannot undo its consumer's omit
      shouldOmit =
        {
          omit,
          side,
          name,
        }:
        let
          go =
            layers:
            layers != [ ]
            && !(matchesInput {
              rules = (head layers).kept;
              inherit side name;
            })
            && (
              matchesInput {
                rules = (head layers).omitted;
                inherit side name;
              }
              || go (tail layers)
            );
        in
        go omit.layers;

      omittedInput =
        { side, name }:
        warn "tack: ${side} input '${name}' was omitted by omit_inputs, ${name} will be null" null;

      # follows paths walk these nodes rather than flake results, since reading a
      # result's `inputs` forces its outputs, which may depend on the follower
      inputNode = value: {
        inherit value;
        inputs = mapAttrs (_: inputNode) (value.inputs or { });
      };

      # `pin/input/...` walks the inputs that pin was evaluated with, as a flake.nix follows does
      resolveFollows = mapAttrs (
        _: target:
        let
          path = filter isString (split "/" target);
          name = head path;
          pin =
            nodes.${name} or (
              if
                any (alias: all_follow.${alias} == name && substring 0 5 alias != "tack:") (attrNames all_follow)
              then
                throw "tack: all_follow target '${name}' has no lock entry; run tack update"
              else
                throw "tack: follows target '${name}' is not a pin"
            );
        in
        foldl' (
          node: key: node.inputs.${key} or (throw "tack: follows target '${target}' has no input '${key}'")
        ) pin (tail path)
      );

      # follow keys are `flake:name`, `tack:name`, or bare `name`
      # project onto one side, rekeyed to bare names; exclusions apply only
      # to global follows, before level-local follows are merged over them
      projectFollows =
        {
          side,
          follows,
          location,
          excluded ? [ ],
        }:
        let
          checked = checkFollowKeys { inherit follows location; };
        in
        listToAttrs (
          concatMap (
            key:
            let
              m = match "(flake|tack):(.*)" key;
            in
            if
              m == null
              && !(matchesInput {
                rules = excluded;
                inherit side;
                name = key;
              })
            then
              [
                {
                  name = key;
                  value = follows.${key};
                }
              ]
            else if
              m != null
              && head m == side
              && !(matchesInput {
                rules = excluded;
                inherit side;
                name = elemAt m 1;
              })
            then
              [
                {
                  name = elemAt m 1;
                  value = follows.${key};
                }
              ]
            else
              [ ]
          ) (attrNames checked)
        );

      followLayersForSide =
        {
          side,
          policy,
          resolve,
          inherited,
        }:
        resolve (projectFollows {
          inherit side;
          follows = policy.global;
          location = "all_follow";
          excluded = policy.excluded;
        })
        // resolve (projectFollows {
          inherit side;
          follows = policy.local;
          location = policy.localLocation;
        })
        // projectFollows {
          inherit side;
          follows = inherited;
          location = "inherited follows";
        };

      followNodesForSide =
        { side, policy }:
        followLayersForSide {
          inherit side policy;
          resolve = resolveFollows;
          inherited = mapAttrs (_: inputNode) policy.inherited;
        };

      followOverridesForSide = args: mapAttrs (_: node: node.value) (followNodesForSide args);

      followMetaForSide =
        { side, policy }:
        followLayersForSide {
          inherit side policy;
          resolve = resolveFollowMeta;
          inherited = policy.inheritedMeta;
        };

      scopedFollowValues =
        side: values:
        listToAttrs (
          map (name: {
            name = "${side}:${name}";
            value = values.${name};
          }) (attrNames values)
        );

      propagatedPolicy =
        {
          omit,
          follows,
          omitted,
          meta,
        }:
        let
          followValues =
            scopedFollowValues "flake" (followOverridesForSide {
              side = "flake";
              policy = follows;
            })
            // scopedFollowValues "tack" (followOverridesForSide {
              side = "tack";
              policy = follows;
            });
          active = any (layer: layer.omitted != [ ] || layer.kept != [ ]) omit.layers || followValues != { };
        in
        {
          inherit active;
          override = {
            __tack_policy = {
              inherit omit omitted meta;
              fetchMode = forcedMode;
              follows = followValues;
              followMeta = scopedFollowValues "tack" (followMetaForSide {
                side = "tack";
                policy = follows;
              });
            };
          };
        };

      omitOverridesFor =
        {
          side,
          inputs,
          omit,
        }:
        listToAttrs (
          map (name: {
            inherit name;
            value = omittedInput { inherit side name; };
          }) (filter (name: shouldOmit { inherit omit side name; }) (attrNames inputs))
        );

      tackCallFor =
        {
          dir,
          omit,
          levelFollows,
          deepFollows,
        }:
        let
          tackPinsPath = dir + "/.tack/pins.toml";
          hasTack = pathExists tackPinsPath;
          upPins = if hasTack then fromTOML (readFile tackPinsPath) else { };
          tackInputs = upPins.inputs or { };
          tackOverrides = intersectAttrs tackInputs (followOverridesForSide {
            side = "tack";
            policy = levelFollows;
          });
          tackOmitOverrides = omitOverridesFor {
            side = "tack";
            inputs = tackInputs;
            inherit omit;
          };
          effectiveTackOverrides = tackOmitOverrides // tackOverrides;
          propagated = propagatedPolicy {
            inherit omit;
            follows = deepFollows;
            omitted = attrNames (removeAttrs tackOmitOverrides (attrNames tackOverrides));
            meta = intersectAttrs tackInputs (followMetaForSide {
              side = "tack";
              policy = levelFollows;
            });
          };
          supportsOverrides = (upPins.tack or { }).recomposable or false;
          direct = effectiveTackOverrides != { };
          needed = direct || propagated.active;
          # policy for deeper levels may match nothing below, so only a direct override warns
          blocked = hasTack && direct && !supportsOverrides;
        in
        {
          inherit supportsOverrides direct needed;
          overrides = effectiveTackOverrides // propagated.override;
          report =
            value:
            if blocked then
              trace "tack: ${dir}: not marked recomposable (set [tack] recomposable = true); overrides will not reach upstream" value
            else
              value;
        };

      mkCallerNodes =
        {
          upLock,
          nodeName,
          rawInputs,
          levelOverrides,
          selfFollowed,
          rootInputs,
          deepFollows,
          omit,
        }:
        mapAttrs (
          n: _decl:
          levelOverrides.${n} or (
            if
              !(elem n selfFollowed)
              && shouldOmit {
                inherit omit;
                side = "flake";
                name = n;
              }
            then
              inputNode (omittedInput {
                side = "flake";
                name = n;
              })
            else if upLock != null then
              let
                ref =
                  (upLock.nodes.${nodeName}.inputs or { }).${n}
                    or (throw "tack: input '${n}' declared but not in flake.lock node '${nodeName}'");
                # a lock follows path names the root's input, which may itself be followed or omitted
                followed = foldl' (
                  node: key:
                  node.inputs.${key} or (throw "tack: follows path dead-end: no input '${key}' along ${toString ref}")
                ) rootInputs.${head ref} (tail ref);
                childName = resolveSpec {
                  inherit upLock;
                  spec = ref;
                };
                childNode = upLock.nodes.${childName};
                childSrc = fetchLocked {
                  what = "flake.lock node '${childName}'";
                  locked = removeAttrs childNode.locked [ "dir" ];
                };
              in
              if isList ref && ref != [ ] then
                followed
              else if childNode.flake or true then
                evalTransitive {
                  inherit upLock rootInputs omit;
                  nodeName = childName;
                  sourceInfo = childSrc;
                  dir = childNode.locked.dir or null;
                  follows = deepFollows;
                }
              else if childNode.locked ? dir then
                inputNode (
                  childSrc
                  // {
                    outPath = childSrc.outPath + "/" + childNode.locked.dir;
                    sourceInfo = childSrc;
                  }
                )
              else
                inputNode childSrc
            else
              throw "tack: no flake.lock; cannot resolve input '${n}'"
          )
        ) rawInputs;

      mkFlakeResult =
        {
          sourceInfo,
          flakeDir,
          callerInputs,
          outputs,
        }:
        outputs
        // sourceInfo
        // {
          outPath = flakeDir;
          inputs = callerInputs;
          inherit outputs sourceInfo;
          _type = "flake";
        };

      evalFlake =
        {
          sourceInfo,
          flakeDir,
          upLock,
          nodeName,
          levelFollows,
          deepFollows,
          omit,
          rootInputs ? null,
        }:
        let
          raw = import (flakeDir + "/flake.nix");

          # project follows onto each side, keep only names that side has
          # bare follow reaches both; `flake:`/`tack:` reaches just one
          tackCall = tackCallFor {
            dir = flakeDir;
            inherit omit levelFollows deepFollows;
          };
          flakeLevel = intersectAttrs (raw.inputs or { }) (followNodesForSide {
            side = "flake";
            policy = levelFollows;
          });

          # deep follows pass down raw, so each descendant re-projects per side
          callerNodes = mkCallerNodes {
            inherit
              upLock
              nodeName
              deepFollows
              omit
              ;
            rawInputs = raw.inputs or { };
            levelOverrides = flakeLevel;
            selfFollowed = levelFollows.selfFollowed or [ ];
            rootInputs = if rootInputs == null then callerNodes else rootInputs;
          };
          callerInputs = mapAttrs (_: node: node.value) callerNodes;

          # upstream declares its outputs forward tackOverrides; a closed `{ self }:`
          # would throw on the extra kwarg, so forward only when declared
          extraArgs =
            if tackCall.supportsOverrides && (tackCall.needed || forcedMode != null) then
              { tackOverrides = tackCall.overrides; }
            else
              { };

          outputs = raw.outputs (callerInputs // extraArgs // { self = result; });

          result = tackCall.report (mkFlakeResult {
            inherit
              sourceInfo
              flakeDir
              callerInputs
              outputs
              ;
          });
        in
        {
          value = result;
          inputs = callerNodes;
        };

      evalTransitive =
        {
          upLock,
          nodeName,
          sourceInfo,
          dir,
          follows,
          omit,
          rootInputs,
        }:
        evalFlake {
          inherit
            upLock
            nodeName
            sourceInfo
            omit
            rootInputs
            ;
          flakeDir = sourceInfo.outPath + (if dir == null then "" else "/" + dir);
          levelFollows = follows;
          deepFollows = follows;
        };

      evalTopFlake =
        {
          sourceInfo,
          name,
          pin,
          omit,
        }:
        let
          flakeDir = sourceInfo.outPath + (if pin ? dir then "/" + pin.dir else "");
          upLockPath = flakeDir + "/flake.lock";
          upLock = if pathExists upLockPath then fromJSON (readFile upLockPath) else null;
          rootNode = if upLock != null then upLock.root else null;
          f = followsFor { inherit name pin; };
        in
        builtins.seq omit (evalFlake {
          inherit
            sourceInfo
            flakeDir
            upLock
            omit
            ;
          nodeName = rootNode;
          levelFollows = f.level;
          deepFollows = f.deep;
        });

      evalFetch =
        {
          sourceInfo,
          name,
          pin,
          subdir,
          omit,
        }:
        let
          path = sourceInfo.outPath + subdir;
          f = followsFor { inherit name pin; };
          # a fetch drill-in is tack-only
          tackCall = tackCallFor {
            dir = path;
            inherit omit;
            levelFollows = f.level;
            deepFollows = f.deep;
          };
        in
        # only override tack files within a `fetch`, since there's no flake.lock
        builtins.seq omit (
          if tackCall.supportsOverrides && tackCall.needed then
            let
              upstream = import (path + "/.tack");
            in
            # old resolvers return a plain attrset, not a callable functor
            if upstream ? __functor then
              (upstream { inherit (tackCall) overrides; }) // { outPath = path; }
            else if tackCall.direct then
              trace "tack: ${path}: upstream .tack predates override support; overrides will not reach it" path
            else
              path
          else
            tackCall.report path
        );

      loadPin =
        { name, pin }:
        let
          pinType =
            pin.type or (
              if pin ? flake then (if pin.flake then "flake" else "fetch") else (pins.tack or { }).type or "flake"
            );
          omit = omitsFor { inherit name pin; };
          follows = checkFollowKeys {
            follows = pin.follows or { };
            location = "inputs.${name}.follows";
          };
          excludeFollow = validateInputNames {
            value = pin.exclude_follow or [ ];
            location = "inputs.${name}.exclude_follow";
          };
        in
        builtins.deepSeq
          [
            omit
            follows
            excludeFollow
          ]
          (
            if pinType == "fixed" then
              inputNode (fetchFixed {
                inherit name;
                entry = lock.${name};
              })
            else
              let
                sourceInfo =
                  if (pin.patches or [ ]) == [ ] then fetchPin name else fetchPatched { inherit name pin; };
                subdir = if pin ? dir then "/" + pin.dir else "";
                node = lock.${name} or { };
              in
              if pinType == "flake" then
                evalTopFlake {
                  inherit
                    sourceInfo
                    name
                    pin
                    omit
                    ;
                }
              # archive is only locked for trees without .tack
              else if
                pinFetchMode pin == "build" && node.archive or null != null && (pin.patches or [ ]) == [ ]
              then
                inputNode (fetchArchive node + subdir)
              else
                inputNode (evalFetch {
                  inherit
                    sourceInfo
                    name
                    pin
                    subdir
                    omit
                    ;
                })
          );

      # undeclared lock entries are synthesised into toplevels by auto-dedup
      # only when referenced as [all_follow] targets
      autoTargets = listToAttrs (
        map (target: {
          name = target;
          value = true;
        }) (attrValues all_follow)
      );
      autoNames = filter (n: !(declared ? ${n}) && !(topFollows ? ${n}) && autoTargets ? ${n}) (
        attrNames lock
      );
      autoPin =
        name:
        let
          sourceInfo = fetchPin name;
          pin = { };
          omit = omitsFor { inherit name pin; };
        in
        if pathExists (sourceInfo.outPath + "/flake.nix") then
          evalTopFlake {
            inherit
              sourceInfo
              name
              pin
              omit
              ;
          }
        else
          inputNode sourceInfo;

      nodes =
        (mapAttrs (name: pin: loadPin { inherit name pin; }) declared)
        // listToAttrs (
          map (name: {
            inherit name;
            value = autoPin name;
          }) autoNames
        )
        // resolveFollows topFollows
        // mapAttrs (_: inputNode) pinOverrides;

      self = mapAttrs (_: node: node.value) nodes;

      selfMeta = removeAttrs (
        mapAttrs (name: _: pinMeta name) (removeAttrs self (attrNames pinOverrides ++ attrNames topFollows))
        // mapAttrs (_: target: {
          type = "follows";
          follows = target;
        }) (removeAttrs topFollows (attrNames pinOverrides))
        // mapAttrs (name: _: inheritedOverrideMeta.${name} or { type = "upstream"; }) pinOverrides
      ) inheritedOmitted;
    in
    builtins.seq all_omit_inputs (
      builtins.seq
        (checkFollowKeys {
          follows = all_follow;
          location = "all_follow";
        })
        (
          self
          // {
            _meta = selfMeta;
            _patched = ownPatched // {
              _nested = nestedPatched // {
                recurseForDerivations = true;
              };
            };
            __functor = _: { ... }@args: call ({ inherit resolverDir; } // args);
          }
        )
    );
in
call { }
