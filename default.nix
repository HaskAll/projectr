{ pkgs ? import <nixpkgs> {} }:

with pkgs.lib;

let
  self = rec {

    projectr = self;

    callPackage = pkgs.newScope self;

    fetchCabal = { cabal2nix, fetchgit, fetchFromGitHub, fetchzip, haskellPackages, runCommand, ... }:
      let
        srcPackage = src:
          runCommand "src-package"
            {
              inherit src;
              buildInputs = [ cabal2nix ];
            }
            ''
              mkdir -p $out
              cp -r $src/* $out
              cd $out
              cabal2nix . > package.nix
            '';
        fetch = src: haskellPackages.callPackage "${srcPackage src}/package.nix" {};
      in {
        inherit fetch;
        fromGit = url: rev: sha256: fetch (fetchgit {
          inherit url rev sha256;
        });
        fromGitHub = owner: repo: rev: sha256: fetch (fetchFromGitHub {
          inherit owner repo rev sha256;
        });
        fromZip = url: sha256: fetch (fetchzip {
          inherit url sha256;
        });
      };

    haskellPackageOverrides = self: super: let
      fetchCabal' = callPackage fetchCabal { haskellPackages = self; };
      reflex-dom-all = import (pkgs.fetchFromGitHub {
        owner = "reflex-frp";
        repo = "reflex-dom";
        rev = "1cdea3187bfadda0ffb1ba680a1c0c4260bc4fc6";
        sha256 = "1195gsw67r4ld8k0b4hh2h344jiq4k9fa32z2m92c9m4pqwgfp5m";
      }) self pkgs;
    in with pkgs.haskell.lib;
     {

      projectr-api = import ./api { inherit projectr; haskellPackages = self; }; 
      projectr-backend = import ./backend { inherit projectr; haskellPackages = self; }; #fetchCabal'.fetch ./backend;
      projectr-frontend = import ./frontend { inherit projectr; haskellPackages = self; }; #fetchCabal'.fetch ./frontend;

      distributed-process-async = doJailbreak super.distributed-process-async;
      distributed-process-client-server = doJailbreak super.distributed-process-client-server;
      distributed-process-extras = doJailbreak super.distributed-process-extras;
      distributed-process-systest = doJailbreak super.distributed-process-systest;
      extended-reals = dontCheck super.extended-reals;

      # project-m36 = dontCheck super.project-m36;
      project-m36 = dontCheck (fetchCabal'.fetch ../../mandco/project-m36);

      reflex-dom-core = reflex-dom-all.reflex-dom-core;
      reflex-dom = reflex-dom-all.reflex-dom;

      jsaddle = addBuildDepend super.jsaddle self.ghcjs-base;

      reflex = addBuildDepend (self.callPackage (pkgs.fetchFromGitHub {
        owner = "reflex-frp";
        repo = "reflex";
        rev = "5add4130db67159fc08b90f795d38d8ba15bb437";
        sha256 = "1ivz4ci62pcix7qfmijmzr1q8f8cgm90c6kvyzmi9nc6rz3xibdm";
      }) {}) self.ghcjs-base;
    };

    haskellPackages = haskell.packages.ghc802;

    haskell.packages =
      let
        overridePackageSet = p: p.override {
          overrides = haskellPackageOverrides;
        };
      in mapAttrs (n: p: overridePackageSet p) pkgs.haskell.packages;

  };

in self
