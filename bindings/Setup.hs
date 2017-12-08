import Mapnik.Setup (defaultMapnikMainWith)
import Distribution.PackageDescription

main = defaultMapnikMainWith $ \bi -> bi {
  extraLibs = "mapnik-wkt":extraLibs bi
  }

