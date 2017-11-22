#ifndef HS_DATASOURCE_HPP
#define HS_DATASOURCE_HPP

#include <HsFFI.h>
#include <mapnik/datasource.hpp>
#include <mapnik/feature_layer_desc.hpp>

#include "HsFFI.h"
#include "hs_featureset.hpp"

using feature_list = mapnik::hs_featureset::feature_list;

namespace mapnik {

typedef HsStablePtr (*features_callback)(context_ptr*,feature_list*, query const *q);
typedef HsStablePtr (*features_at_point_callback)(context_ptr*,feature_list*, double x, double y, double tol);

class MAPNIK_DECL hs_datasource : public datasource
{
  friend class hs_featureset;
public:

  hs_datasource( std::string const& layer_name
               , datasource::datasource_t type
               , boost::optional<datasource_geometry_t> 
               , box2d<double> const& extent
               , features_callback
               , features_at_point_callback
               );

  static const char * name();
  virtual ~hs_datasource();
  virtual datasource::datasource_t type() const;
  virtual featureset_ptr features(query const& q) const;
  virtual featureset_ptr features_at_point(coord2d const& pt, double tol = 0) const;
  virtual box2d<double> envelope() const;
  virtual boost::optional<datasource_geometry_t> get_geometry_type() const;
  virtual layer_descriptor get_descriptor() const;
  void  push_key(std::string const&);
private:
  const mapnik::layer_descriptor desc_;
  const datasource::datasource_t type_;
  const box2d<double> extent_;
  const boost::optional<datasource_geometry_t> geom_type_;
  features_callback get_features_;
  features_at_point_callback get_features_at_point_;
  context_ptr ctx_;
};

}

#endif // HS_DATASOURCE_HPP
