self: super: {}

#rec {
  #manix = super.manix.overrideAttrs (
    #old: {
      #src = builtins.fetchTarball {
        #name   = "manix-v0.6.2-2020-10-18";
        #url    = "https://github.com/mlvzk/manix/archive/7e905be.tar.gz";
        #sha256 = "1sjwlck3r83hy9qzzw1kjyfnd2r0pgrdl0km6lfmbj7xwshfd0as";
      #};
    #}
  #);
#}
