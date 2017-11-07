#include <mapnik/map.hpp>
#include <mapnik/image.hpp>
#include <mapnik/layer.hpp>
#include <mapnik/datasource.hpp>
#include <mapnik/expression.hpp>
#include <mapnik/color.hpp>
#include <mapnik/projection.hpp>
#include <mapnik/proj_transform.hpp>
#include <mapnik/feature_type_style.hpp>
#include <mapnik/rule.hpp>
#include <mapnik/symbolizer_base.hpp>

extern "C" {

MAPNIK_DECL void hs_mapnik_destroy_Image(void *p) {
  delete static_cast<mapnik::image_rgba8*>(p);
}

MAPNIK_DECL void hs_mapnik_destroy_Map(void *p) {
  delete static_cast<mapnik::Map*>(p);
}

MAPNIK_DECL void hs_mapnik_destroy_Layer(void *p) {
  delete static_cast<mapnik::layer*>(p);
}

MAPNIK_DECL void hs_mapnik_destroy_Datasource(void *p) {
  delete static_cast<mapnik::datasource_ptr*>(p);
}

MAPNIK_DECL void hs_mapnik_destroy_Parameters(void *p) {
  delete static_cast<mapnik::parameters*>(p);
}

MAPNIK_DECL void hs_mapnik_destroy_Projection(void *p) {
  delete static_cast<mapnik::projection*>(p);
}

MAPNIK_DECL void hs_mapnik_destroy_ProjTransform(void *p) {
  delete static_cast<mapnik::proj_transform*>(p);
}

MAPNIK_DECL void hs_mapnik_destroy_Color(void *p) {
  delete static_cast<mapnik::color*>(p);
}

MAPNIK_DECL void hs_mapnik_destroy_Style(void *p) {
  delete static_cast<mapnik::feature_type_style*>(p);
}

MAPNIK_DECL void hs_mapnik_destroy_Rule(void *p) {
  delete static_cast<mapnik::rule*>(p);
}

MAPNIK_DECL void hs_mapnik_destroy_Symbolizer(void *p) {
  delete static_cast<mapnik::symbolizer*>(p);
}

MAPNIK_DECL void hs_mapnik_destroy_Expression(void *p) {
  delete static_cast<mapnik::expression_ptr*>(p);
}
}

