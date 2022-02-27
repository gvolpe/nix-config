self: super:

{
  mtxclient = super.mtxclient.overrideAttrs (
    old: rec {
      version = "0.6.2";

      src = super.fetchFromGitHub {
        owner = "Nheko-Reborn";
        repo = "mtxclient";
        rev = "v${version}";
        sha256 = "sha256-TsGoSVewQJlr0zj8qYEd+UU8DlncZDCqfrqTv89LEYU=";
      };
    }
  );

  nheko = super.nheko.overrideAttrs (
    old: rec {
      version = "0.9.1-1";

      src = super.fetchFromGitHub {
        owner = "Nheko-Reborn";
        repo = "nheko";
        rev = "v${version}";
        sha256 = "sha256-GZjGDibw5dE72R8kRBOd8Ylz+LrDOK0mgrwd2cUkB70=";
      };
    }
  );
}
