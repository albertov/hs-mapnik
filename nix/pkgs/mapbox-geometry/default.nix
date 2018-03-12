{ writeScript, stdenv, fetchzip }:

stdenv.mkDerivation rec {
  name = "mapbox-geometry-${version}";
  version = "0.9.2";

  src = fetchzip {
    url = "https://github.com/mapbox/geometry.hpp/archive/v${version}.tar.gz";
    sha256 = "1xzylqz1rvm1ha7c1gfsvv9w92v4jzg8rqqf177i39kaqdz8xfad";
  };

  configurePhase = " ";
  buildPhase = " ";
  installPhase = ''
    mkdir $out
    cp -a include $out/
    '';

  meta = with stdenv.lib; {
    description = "C++ geometry types";
    homepage = https://github.com/mapbox/geometry.hpp;
    license = licenses.isc;
    platforms = platforms.all;
  };
}

