{ stdenv, fetchzip, cmake, protobuf
, enableDebug ? false
}:

stdenv.mkDerivation rec {
  name = "protozero-${version}";
  version = "1.6.1";

  src = fetchzip {
    # this one contains all git submodules and is cheaper than fetchgit
    url = "https://github.com/mapbox/protozero/archive/v${version}.tar.gz";
    sha256 = "1kr1zf7r57i1ari96ly9h2x0811ryrknvxmww16ilbr9im7dnpw8";
  };

  dontStrip = enableDebug;

  nativeBuildInputs = [ cmake ];
  cmakeFlags = [];

  enableParallelBuilding = true;

  meta = with stdenv.lib; {
    description = "Minimalistic protocol buffer decoder and encoder in C++";
    homepage = https://github.com/mapbox/protozero;
    license = licenses.bsd2;
    platforms = platforms.all;
  };
}

