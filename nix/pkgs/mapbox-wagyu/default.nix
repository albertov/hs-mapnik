{ writeScript, stdenv, fetchzip }:

stdenv.mkDerivation rec {
  name = "mapbox-wagyu-${version}";
  version = "0.4.3";

  src = fetchzip {
    url = "https://github.com/mapbox/wagyu/archive/${version}.tar.gz";
    sha256 = "00li5b3qwz921gry7px4rri0lgyhdbphsx9mr6a63zl4xkr9c1bv";
  };

  configurePhase = " ";
  buildPhase = " ";
  installPhase = ''
    mkdir $out
    cp -a include $out/
    '';

  meta = with stdenv.lib; {
    description = "A general library for geometry operations of union,
    intersections, difference, and xor";
    homepage = https://github.com/mapbox/wagyu;
    license = licenses.isc;
    platforms = platforms.all;
  };
}

