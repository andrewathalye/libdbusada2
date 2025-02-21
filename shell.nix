{ nix-ada }:

nix-ada.pkgs.mkShell {
   nativeBuildInputs = [
      nix-ada.pkgs.gprbuild  
      nix-ada.pkgs.gnat
      nix-ada.libadalang-tools
      nix-ada.gnatformat
      nix-ada.ada-language-server
      nix-ada.pkgs.nodejs

      # Debugging and tests
      nix-ada.pkgs.gdb
      nix-ada.pkgs.socat
#      nix-ada.pkgs.gnatprove
#      nix-ada.pkgs.lcov
#      nix-ada.pkgs.valgrind
#      nix-ada.pkgs.rr
#      nix-ada.gnatcoverage
   ];
      
   shellHook = ''
      export LIBRARY_TYPE=relocatable
   '';
}
