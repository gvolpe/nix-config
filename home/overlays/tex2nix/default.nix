self: super:

let
  source = builtins.fetchTarball {
    name   = "tex2nix-05-03-2021";
    url    = "https://github.com/Mic92/tex2nix/archive/4b17bc0.tar.gz";
    sha256 = "15j2zqnvvb1bd3n1dj9zw9h42b4zv3izzvgnl4r8yx2qm2vkcisc";
  };
in
{
  tex2nix = self.callPackage (import source) {};
}
