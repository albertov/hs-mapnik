{ stdenv, fetchgit, mapnik, zlib, protobuf, python2Packages, icu
, boost, mapbox-geometry, protozero, mapbox-wagyu, harfbuzz
, libjpeg, libpng, cairo, libtiff, libxml2, proj, libwebp
, enableDebug ? false
}:
let buildType = if enableDebug then "Debug" else "Release";
in stdenv.mkDerivation rec {
  name = "mapnik-vector-tile-${version}";
  version = "1.5.0";

  src = fetchgit {
    url = https://github.com/mapbox/mapnik-vector-tile;
    rev = "259ff557f1757c4bd6ecc7350bcd278514bbdfa7";
    sha256 =  "1wg1nijxxn92lyr5s4fxm1ln725y1jd1xi5jwkm12whk9h9wvq8d";
  };

  patches = [ ./make_shared_library.patch ];

  dontStrip = enableDebug;
  doCheck = true;

  outputs = [ "out" ];

  configurePhase = ''
    gyp gyp/build.gyp \
      --depth=. \
      -DMAPNIK_PLUGINDIR="\"$(mapnik-config --input-plugins)\"" \
      -Goutput_dir=. \
      --generator-output=./build \
      -f make
    '';

  buildPhase = ''
    BUILDTYPE=${buildType} make -j$NIX_BUILD_CORES -C build V=1
    '';

  checkPhase = ''
    ./build/${buildType}/tests
  '';

  installPhase = ''
    mkdir -p $out/{include,lib,bin}
    install src/*.hpp $out/include
    install build/${buildType}/obj/gen/vector_tile.pb.h $out/include
    install build/${buildType}/lib.target/*.so $out/lib
    for exe in tileinfo vtile-decode vtile-edit vtile-encode vtile-transform; do
      install build/${buildType}/$exe $out/bin
    done
    '';

  nativeBuildInputs = [ python2Packages.gyp protobuf ];
  #propagatedNativeBuildInputs =
  #  [ mapnik zlib protozero mapbox-geometry mapbox-wagyu ];
  buildInputs =
    [ mapnik boost icu harfbuzz zlib protozero
      mapbox-geometry mapbox-wagyu
      libjpeg libpng libtiff libxml2 proj cairo libwebp ];

  enableParallelBuilding = true;


  meta = with stdenv.lib; {
    description = "An open source toolkit for developing mapping applications";
    homepage = http://mapnik.org;
    license = licenses.lgpl21;
    platforms = platforms.all;
  };
}
