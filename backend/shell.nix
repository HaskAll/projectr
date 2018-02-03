{ projectr ? import ../. {} }:
with projectr;
(import ./. { inherit projectr; inherit haskellPackages; }).env
