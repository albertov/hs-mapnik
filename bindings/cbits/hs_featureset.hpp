#ifndef HS_FEATURESET_HPP
#define HS_FEATURESET_HPP

#include <mapnik/box2d.hpp>
#include <mapnik/featureset.hpp>
#include <mapnik/datasource.hpp>
#include <mapnik/feature.hpp>

#include <vector>

namespace mapnik {

class hs_featureset : public Featureset
{
public:
  using feature_list = std::vector<feature_ptr>;
  using feature_list_ptr = std::shared_ptr<feature_list>;

  hs_featureset(feature_list_ptr const& features, datasource::datasource_t type)
    : features_(features),
      pos_(features->begin()),
      end_(features->end()),
      type_(type)
  {}

  virtual ~hs_featureset() {}

  feature_ptr next()
  {
    while (pos_ != end_)
    {
        return *pos_++;
    }
    return feature_ptr();
  }

private:
  const feature_list_ptr features_;
  mutable feature_list::const_iterator pos_;
  const feature_list::const_iterator end_;
  const datasource::datasource_t type_;
};
}

#endif // HS_FEATURESET_HPP
