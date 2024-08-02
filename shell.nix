{
  system ? builtins.currentSystem,
  compiler ? null,
}: let
  pkgs = import ./nix {inherit system compiler;};
in
  pkgs.mkShell {
    buildInputs = [
      pkgs.kami-frontend-hs.shell
    ];
    shellHook = ''
      export LD_LIBRARY_PATH=${pkgs.kami-frontend-hs.shell}/lib:$LD_LIBRARY_PATH
      logo
    '';
  }
