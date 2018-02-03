{ projectr ? import ../. {}, haskellPackages }:
with projectr;
(callPackage fetchCabal { inherit haskellPackages; }).fetch ./.
