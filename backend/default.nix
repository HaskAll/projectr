{ projectr ? import ../. {}, nixpkgs ? import <nixpkgs> {}, haskellPackages }:
with projectr;
let
  drv = (callPackage fetchCabal { inherit haskellPackages; }).fetch ./.;
  extraAttrs = { PROJECTR_FRONTEND = haskell.packages.ghcjsHEAD.projectr-frontend; };
in
drv.overrideAttrs (attrs: extraAttrs) // {
  env = drv.env.overrideAttrs (attrs: extraAttrs);
}
