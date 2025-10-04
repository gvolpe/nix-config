{ pkgs, ... }:

let
  pname = "nfsm";
  nfsm = pkgs.python3Packages.buildPythonApplication {
    inherit pname;
    version = "0.0.1";
    pyproject = false;
    propagatedBuildInputs = [ ];
    dontUnpack = true;
    installPhase = "install -Dm755 ${./${pname}.py} $out/bin/${pname}";
  };
in
{
  home.packages = [ nfsm ];
}
