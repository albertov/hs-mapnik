#include <mapnik/map.hpp>
#include <mapnik/image.hpp>
#include <mapnik/layer.hpp>
#include <mapnik/datasource.hpp>
#include <mapnik/expression.hpp>
#include <mapnik/projection.hpp>
#include <mapnik/proj_transform.hpp>
#include <mapnik/transform_expression.hpp>
#include <mapnik/feature_type_style.hpp>
#include <mapnik/rule.hpp>
#include <mapnik/symbolizer_base.hpp>
#include <mapnik/raster_colorizer.hpp>
#include <mapnik/text/text_properties.hpp>
#include <mapnik/feature.hpp>

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

MAPNIK_DECL void hs_mapnik_destroy_Transform(void *p) {
  delete static_cast<mapnik::transform_type*>(p);
}

MAPNIK_DECL void hs_mapnik_destroy_Colorizer(void *p) {
  delete static_cast<mapnik::raster_colorizer_ptr*>(p);
}

MAPNIK_DECL void hs_mapnik_destroy_TextPlacements(void *p) {
  delete static_cast<mapnik::text_placements_ptr*>(p);
}

MAPNIK_DECL void hs_mapnik_destroy_GroupProperties(void *p) {
  delete static_cast<mapnik::group_symbolizer_properties_ptr*>(p);
}

MAPNIK_DECL void hs_mapnik_destroy_TextSymProperties(void *p) {
  delete static_cast<mapnik::text_symbolizer_properties*>(p);
}

MAPNIK_DECL void hs_mapnik_destroy_Format(void *p) {
  delete static_cast<mapnik::formatting::node_ptr*>(p);
}

MAPNIK_DECL void hs_mapnik_destroy_SymbolizerValue(void *p) {
  delete static_cast<mapnik::symbolizer_base::value_type*>(p);
}

MAPNIK_DECL void hs_mapnik_destroy_Feature(void *p) {
  delete static_cast<mapnik::feature_ptr*>(p);
}
}

