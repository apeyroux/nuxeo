{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, attoparsec, base, bytestring, conduit
      , conduit-extra, hpack, http-conduit, http-types
      , optparse-applicative, stdenv, text, time, url
      }:
      mkDerivation {
        pname = "nuxeo";
        version = "0.3.0.3";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          aeson attoparsec base bytestring conduit conduit-extra http-conduit
          http-types text time url
        ];
        libraryToolDepends = [ hpack ];
        executableHaskellDepends = [ base optparse-applicative text ];
        preConfigure = "hpack";
        homepage = "https://github.com/apeyroux/nuxeo#readme";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
