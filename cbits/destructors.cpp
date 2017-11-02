#include <mapnik/map.hpp>
#include <mapnik/image.hpp>
#include <mapnik/layer.hpp>
#include <mapnik/datasource.hpp>
#include <mapnik/projection.hpp>
#include <mapnik/proj_transform.hpp>

extern "C" {

void hs_mapnik_destroy_Image(void *p) {
  delete static_cast<mapnik::image_rgba8*>(p);
}

void hs_mapnik_destroy_Map(void *p) {
  delete static_cast<mapnik::Map*>(p);
}

void hs_mapnik_destroy_Layer(void *p) {
  delete static_cast<mapnik::layer*>(p);
}

void hs_mapnik_destroy_Datasource(void *p) {
  delete static_cast<mapnik::datasource_ptr*>(p);
}

void hs_mapnik_destroy_Parameters(void *p) {
  delete static_cast<mapnik::parameters*>(p);
}

void hs_mapnik_destroy_Projection(void *p) {
  delete static_cast<mapnik::projection*>(p);
}

void hs_mapnik_destroy_ProjTransform(void *p) {
  delete static_cast<mapnik::proj_transform*>(p);
}

}

