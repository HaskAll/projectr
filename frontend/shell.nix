{ projectr ? import ../. {} }:
(import ./. { inherit projectr; }).env
