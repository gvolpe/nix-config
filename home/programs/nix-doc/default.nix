{ rustPlatform }:

rustPlatform.buildRustPackage rec {
  pname   = "nix-doc";
  version = "v0.3.1";

  src = builtins.fetchTarball {
    url    = "https://github.com/lf-/nix-doc/archive/${version}.tar.gz";
    sha256 = "1hiz0fsbsl74m585llg2n359gs8i2m6jh8zdikkwxd8xq7qmw032";
  };

  cargoSha256 = "06b36jv8hk4gqqm4nbh9gfszncvixs3zcjph45wlx7z5m61y9sdg";

  preBuild  = "cd nix-doc/";
  postBuild = "cd ..";

  doCheck = false;
}
