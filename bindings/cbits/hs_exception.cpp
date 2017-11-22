#include "hs_exception.hpp"

namespace mapnik
{

hs_exception::hs_exception(HsStablePtr ptr) : exc_(ptr) {};

HsStablePtr hs_exception::getStablePtr()
{
  return exc_;
}

}
