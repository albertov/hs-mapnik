#include <mapnik/symbolizer.hpp>
#include <mapnik/symbolizer_base.hpp>

// stl
#include <string>

using dash_t = std::pair<double,double>;

using namespace mapnik;

struct symbolizer_base_extractor
{
    symbolizer_base_extractor(symbolizer_base **ref)
        :ref_(ref) {}

    template <class T>
    void operator() (T const& val) const
    {
      *ref_ = new symbolizer_base (val);
    }

    symbolizer_base **ref_;
};


symbolizer_base *get_symbolizer_base (symbolizer const& sym)
{
  symbolizer_base *ret=NULL;

  util::apply_visitor(symbolizer_base_extractor(&ret), sym);
  return ret;
}

