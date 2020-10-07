{ mkDerivation, achille, aeson, base, binary, bytestring
, containers, data-default, directory, feed, filepath, lucid
, optparse-applicative, pandoc, pandoc-types, process, sort, stdenv
, text, time, yaml
}:
mkDerivation {
  pname = "site";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    achille aeson base binary bytestring containers data-default
    directory feed filepath lucid optparse-applicative pandoc
    pandoc-types process sort text time yaml
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
