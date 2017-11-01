#include <mapnik/map.hpp>
#include <mapnik/image.hpp>

extern "C" {
void hs_mapnik_destroy_Image(void *p) { delete static_cast<mapnik::image_rgba8*>(p); }
void hs_mapnik_destroy_Map(void *p) { delete static_cast<mapnik::Map*>(p); }
}

