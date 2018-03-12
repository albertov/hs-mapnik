self: super:
{
  mapnik = self.callPackage ../pkgs/mapnik {};
  mapbox-geometry = self.callPackage ../pkgs/mapbox-geometry {};
  mapnik-vector-tile = self.callPackage ../pkgs/mapnik-vector-tile {};
  mapbox-wagyu = self.callPackage ../pkgs/mapbox-wagyu {};
  protozero = self.callPackage ../pkgs/protozero {};
}

