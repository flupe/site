{ mkDerivation, achille, aeson, base, binary, bytestring
, containers, data-default, feed, filepath, lucid, pandoc
, pandoc-types, sort, stdenv, text, time, yaml
}:
mkDerivation {
  pname = "site";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    achille aeson base binary bytestring containers data-default feed
    filepath lucid pandoc pandoc-types sort text time yaml
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
