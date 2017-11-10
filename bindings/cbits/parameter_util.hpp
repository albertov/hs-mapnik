#include <mapnik/params.hpp>
#include <mapnik/value_types.hpp>
#include <mapnik/boolean.hpp>
#include <mapnik/util/conversions.hpp>

// stl
#include <string>

using namespace mapnik;

enum class param_type : int {null_type=0, double_type, integer_type, bool_type, string_type};

typedef void (*callback_t)(enum param_type, void *, char *);

struct value_extractor_visitor
{
    value_extractor_visitor(callback_t callback, char *key)
        :callback_(callback), key_(key) {}

    void operator() (value_double const& val) const
    {
      callback_(param_type::double_type, const_cast<void*>(static_cast<const void*>(&val)), key_);
    }

    void operator() (value_integer const& val) const
    {
      callback_(param_type::integer_type, const_cast<void*>(static_cast<const void*>(&val)), key_);
    }

    void operator() (std::string const& val) const
    {
      callback_(param_type::string_type, const_cast<void*>(static_cast<const void*>(strdup(val.c_str()))), key_);
    }

    void operator() (value_bool const& val) const
    {
      int val2 = val;
      callback_(param_type::bool_type, const_cast<void*>(static_cast<const void*>(&val2)), key_);
    }

    void operator() (value_null const& val) const
    {
      callback_(param_type::null_type, NULL, key_);
    }


    callback_t callback_;
    char *key_;
};
