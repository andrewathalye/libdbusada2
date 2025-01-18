{
   #inputs.nix-ada.url = "github:andrewathalye/nix-ada/v1.5";
   inputs.nix-ada.url = "/home/andrew/src/ada/nix-ada";

   outputs = { self, nix-ada }:
   let
      nix-ada_s = nix-ada.packages.x86_64-linux;
   in
   with nix-ada_s;
   {
      devShells.x86_64-linux.default = import nix/shell.nix { nix-ada = nix-ada_s; };
   };
}
