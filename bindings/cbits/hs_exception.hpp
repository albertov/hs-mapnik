#ifndef HS_EXCEPTION_CPP
#define HS_EXCEPTION_CPP

#include <exception>
#include <string>
#include <mapnik/config.hpp>
#include "HsFFI.h"

namespace mapnik {

using HsException = HsStablePtr;

class MAPNIK_DECL hs_exception : public std::exception
{
public:
    hs_exception(HsStablePtr hsException);
    virtual ~hs_exception() {}

    HsStablePtr getStablePtr();
private:
    HsStablePtr exc_;
};

}

#endif // HS_EXCEPTION_CPP
