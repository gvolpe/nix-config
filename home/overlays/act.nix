self: super:

rec {
  act = super.act.overrideAttrs (
    old: {
      version = "0.2.18";
      src = builtins.fetchTarball {
        name   = "act-2020-11-30";
        url    = "https://github.com/nektos/act/archive/d784bce.tar.gz";
        sha256 = "08xm39jipzswa42iipgdd9ykqbry7c7idskqsi51cfgnx14vwidp";
      };
    }
  );
}
