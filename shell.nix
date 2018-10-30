{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, attoparsec, base, bytestring, conduit
      , conduit-extra, hpack, optparse-applicative, stdenv, text, time
      }:
      mkDerivation {
        pname = "nuxeo";
        version = "0.2.0.3";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          attoparsec base bytestring conduit conduit-extra text time
        ];
        libraryToolDepends = [ hpack ];
        executableHaskellDepends = [ base optparse-applicative text ];
        preConfigure = "hpack";
        homepage = "https://github.com/https://github.com/apeyroux/nuxeo#readme";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
