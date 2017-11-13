#include <mapnik/query.hpp>
#include <mapnik/box2d.hpp>
#include <mapnik/boolean.hpp>
#include <mapnik/feature_factory.hpp>

#include "hs_datasource.hpp"
#include "hs_featureset.hpp"

#include <algorithm>

using mapnik::datasource;
using mapnik::parameters;

namespace mapnik {

const char * hs_datasource::name()
{
  return "hs";
}

hs_datasource::hs_datasource( std::string const& layer_name
                            , datasource::datasource_t type
                            , boost::optional<datasource_geometry_t> geom_type
                            , box2d<double> const& extent
                            , features_callback fcb
                            , features_at_point_callback fapcb)
  : datasource(mapnik::parameters()),
    desc_(layer_name, "utf-8"),
    type_(type),
    extent_(extent),
    geom_type_(geom_type),
    get_features_(fcb),
    get_features_at_point_(fapcb)
    {}

hs_datasource::~hs_datasource()
{
  hs_free_fun_ptr(reinterpret_cast<HsFunPtr>(get_features_));
  hs_free_fun_ptr(reinterpret_cast<HsFunPtr>(get_features_at_point_));
}


datasource::datasource_t hs_datasource::type() const
{
  return type_;
}

featureset_ptr hs_datasource::features(const query& q) const
{
  hs_featureset::feature_list_ptr features = std::make_shared<hs_featureset::feature_list>();
  auto ctx = std::make_shared<mapnik::context_type>();
  get_features_(&ctx, features.get(), &q);
  return std::make_shared<hs_featureset>(features, type_);
}


featureset_ptr hs_datasource::features_at_point(coord2d const& pt, double tol) const
{
  hs_featureset::feature_list_ptr features = std::make_shared<hs_featureset::feature_list>();
  auto ctx = std::make_shared<mapnik::context_type>();
  get_features_at_point_(&ctx, features.get(), pt.x, pt.y, tol);
  return std::make_shared<hs_featureset>(features, type_);
}


box2d<double> hs_datasource::envelope() const
{
  return extent_;
}

boost::optional<datasource_geometry_t> hs_datasource::get_geometry_type() const
{
  return geom_type_;
}

layer_descriptor hs_datasource::get_descriptor() const
{
  return desc_;
}


}
