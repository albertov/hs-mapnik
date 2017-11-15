#include <mapnik/unicode.hpp>
#include <mapnik/value.hpp>

// stl
#include <string>

using namespace mapnik;

enum class value_type : int {
  null_type=0
, double_type
, integer_type
, bool_type
, string_type
};

struct value_extractor_visitor
{
    void * const retPtr_;
    int * const tyPtr_;

    value_extractor_visitor(void *retPtr, int *tyPtr)
        :retPtr_(retPtr), tyPtr_(tyPtr) {}

    void operator() (value_double const& val) const
    {
      *tyPtr_ = static_cast<int>(value_type::double_type);
      *static_cast<value_double*>(retPtr_) = val;
    }

    void operator() (value_integer const& val) const
    {
      *tyPtr_ = static_cast<int>(value_type::integer_type);
      *static_cast<value_integer*>(retPtr_) = val;
    }

    void operator() (value_unicode_string const& val) const
    {
      *tyPtr_ = static_cast<int>(value_type::string_type);
      std::string ret;
      to_utf8(val, ret);
      *static_cast<char**>(retPtr_) = strdup(ret.c_str());
    }

    void operator() (value_bool const& val) const
    {
      *tyPtr_ = static_cast<int>(value_type::bool_type);
      *static_cast<int*>(retPtr_) = val;
    }

    void operator() (value_null const& val) const
    {
      *tyPtr_ = static_cast<int>(value_type::null_type);
    }

};
