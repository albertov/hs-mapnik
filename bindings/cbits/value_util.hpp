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
, unicode_string_type
};

struct value_extractor_visitor
{
    void const** const retPtr_;
    value_type * const tyPtr_;
    int        * const lenPtr_;

    value_extractor_visitor(void const** retPtr, value_type *tyPtr, int *lenPtr)
        :retPtr_(retPtr), tyPtr_(tyPtr), lenPtr_(lenPtr) {}

    void operator() (value_null const& val) const
    {
      *tyPtr_  = value_type::null_type;
    }

    void operator() (value_double const& val) const
    {
      *tyPtr_  = value_type::double_type;
      *retPtr_ = &val;
    }

    void operator() (value_integer const& val) const
    {
      *tyPtr_  = value_type::integer_type;
      *retPtr_ = &val;
    }

    void operator() (value_bool const& val) const
    {
      *tyPtr_  = value_type::bool_type;
      *retPtr_ = &val;
    }

    void operator() (std::string const& val) const
    {
      *tyPtr_  = value_type::string_type;
      *retPtr_ = val.c_str();
      *lenPtr_ = val.size();
    }

    void operator() (value_unicode_string const& val) const
    {
      *tyPtr_  = value_type::unicode_string_type;
      *retPtr_ = val.getBuffer();
      *lenPtr_ = val.length();
    }

};
