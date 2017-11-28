import Mapnik.Setup (defaultMapnikMainWith)
import Distribution.PackageDescription

main = defaultMapnikMainWith $ \bi -> bi {
  extraLibs = "mapnik-wkt":"mapnik_vector_tile_impl":"mapnik":"z":extraLibs bi
  }

