#ifndef HS_VECTOR_TILE_DATASOURCE_HPP
#define HS_VECTOR_TILE_DATASOURCE_HPP

#include <vector_tile_datasource_pbf.hpp>

namespace mapnik
{
namespace vector_tile_impl
{

class hs_tile_datasource_pbf : public tile_datasource_pbf
{
public:
  using buffer_ptr = std::shared_ptr<std::string>;
  hs_tile_datasource_pbf(buffer_ptr const& buffer,
                         protozero::pbf_reader const& layer,
                         std::uint64_t x,
                         std::uint64_t y,
                         std::uint64_t z,
                         bool use_tile_extent = false)
    : tile_datasource_pbf(layer,x,y,z,use_tile_extent)
    , buffer_(buffer) {}

private:
  buffer_ptr buffer_;
};

} // namespace vector_tile_impl
} // namespace mapnik

#endif // HS_VECTOR_TILE_DATASOURCE_HPP
