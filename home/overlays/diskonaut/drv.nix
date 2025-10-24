{ lib, stdenv, rustPlatform, sources }:

let
  toml = fromTOML (builtins.readFile "${sources.diskonaut}/Cargo.toml");
in
rustPlatform.buildRustPackage {
  pname = "diskonaut";
  version = toml.package.version;
  src = sources.diskonaut;

  cargoHash = "sha256-XnJbcN+Rcu6v+KDVXa2VzM0oLl8BaDNMIN+/rEHQaDg=";

  doCheck = !stdenv.hostPlatform.isDarwin;

  meta = with lib; {
    description = "Terminal disk space navigator (fork)";
    homepage = "https://github.com/kfkonrad/diskonaut";
    license = licenses.mit;
    maintainers = with maintainers; [ gvolpe ];
    mainProgram = "diskonaut";
  };
}
