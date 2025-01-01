{ nix-ada }:

nix-ada.pkgs.mkShell {
   nativeBuildInputs = [
      nix-ada.gprbuild  
      nix-ada.gnat
      nix-ada.libadalang-tools
      nix-ada.ada-language-server
      nix-ada.pkgs.nodejs
      nix-ada.pkgs.gnatcoll-core

      # Debugging and tests
#      nix-ada.pkgs.gdb
#      nix-ada.pkgs.lcov
#      nix-ada.pkgs.valgrind
#      nix-ada.pkgs.rr
#      nix-ada.gnatcoverage
   ];
      
   shellHook = ''
      export LIBRARY_TYPE=relocatable
   '';
}
