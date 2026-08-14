{
  description = "ppx_rapper development environment";

  inputs.nixpkgs.url = "github:nix-ocaml/nix-overlays";

  outputs =
    { nixpkgs, ... }:
    {
      devShells = nixpkgs.lib.genAttrs nixpkgs.lib.systems.flakeExposed (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          ocamlPackages = pkgs.ocaml-ng.ocamlPackages_5_4;
        in
        {
          default = pkgs.mkShell {
            packages = with ocamlPackages; [
              dune_3
              ocaml
              ocamlformat
            ];
          };
        }
      );
    };
}
