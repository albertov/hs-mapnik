#include <vector_tile_tile.hpp>

extern "C" {

MAPNIK_DECL void hs_mapnik_destroy_Tile(void *p) {
  delete static_cast<mapnik::vector_tile_impl::tile*>(p);
}

}
