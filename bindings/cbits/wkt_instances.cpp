#include <mapnik/wkt/wkt_grammar.hpp>
#include <mapnik/wkt/wkt_grammar_impl.hpp>
#include <mapnik/wkt/wkt_generator_grammar.hpp>
#include <mapnik/wkt/wkt_generator_grammar_impl.hpp>
#include <mapnik/geometry.hpp>
#include <string>

using sink_type = std::back_insert_iterator<std::string>;
using iterator_type = std::string::const_iterator;
using geometry_t = mapnik::geometry::geometry<double>;

// Mapnik does not export these template instantiations that the inlined
// from_wkt and to_wkt need so we instantiate them here
template struct mapnik::wkt::wkt_generator_grammar<sink_type, geometry_t>;
template struct mapnik::wkt::wkt_grammar<iterator_type>;
