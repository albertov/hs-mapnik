#ifndef HS_PROJ_TRANSFORM_HPP
#define HS_PROJ_TRANSFORM_HPP

#include <mapnik/projection.hpp>
#include <mapnik/proj_transform.hpp>
#include <mapnik/util/noncopyable.hpp>

#include <string>

/* We implement this wrapper to hold references to the mapnik::projection
 * objects alive since mapnik::proj_transform takes them by reference
 */
class MAPNIK_DECL hs_proj_transform  : private mapnik::util::noncopyable
{
public:
  hs_proj_transform(std::string const& source, std::string const& dest)
    : src_(source)
    , dst_(dest)
    , trans_(src_, dst_) {};

  mapnik::proj_transform const& trans() const
  {
    return trans_;
  }
private:
  const mapnik::projection src_;
  const mapnik::projection dst_;
  const mapnik::proj_transform trans_;
};
#endif
