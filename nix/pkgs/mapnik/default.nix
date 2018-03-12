{ stdenv, fetchzip
, boost, freetype, gdal, harfbuzz, icu, libjpeg, libpng, libtiff
, libwebp, libxml2, proj, python, sqlite, zlib, postgresql, pkgconfig
}:

stdenv.mkDerivation rec {
  name = "mapnik-${version}";
  version = "3.0.19";

  src = fetchzip {
    # this one contains all git submodules and is cheaper than fetchgit
    url = "https://github.com/mapnik/mapnik/releases/download/v${version}/mapnik-v${version}.tar.bz2";
    sha256 = "0byyc0diy2y6wzqdqcnjad6pdy2dyir04iixpbwxblmidzhqd9g0";
  };

  # a distinct dev output makes python-mapnik fail
  outputs = [ "out" ];

  patches = [
    ./fix-overviews.patch
    ];

  configurePhase = ''
    python scons/scons.py --implicit-deps-changed configure \
      PREFIX="$out" \
      INPUT_PLUGINS=all \
      XMLPARSER=libxml2 \
      FAST=True \
    '';

  buildPhase = ''
    python scons/scons.py -j$NIX_BUILD_CORES --config=cache --implicit-cache --max-drift=1
    '';

  installPhase = ''
    python scons/scons.py -j$NIX_BUILD_CORES --config=cache --implicit-cache --max-drift=1 install
    '';

  # These are propagated because mapnik headers reference them
  # propagatedBuildInputs = [ boost harfbuzz icu ];

  buildInputs =
    [ gdal boost freetype harfbuzz icu libjpeg libpng libtiff
      libwebp libxml2 proj python sqlite zlib pkgconfig
    ];

  enableParallelBuilding = true;


  meta = with stdenv.lib; {
    description = "An open source toolkit for developing mapping applications";
    homepage = http://mapnik.org;
    license = licenses.lgpl21;
    platforms = platforms.all;
  };
}
