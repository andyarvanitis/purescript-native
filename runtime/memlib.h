///////////////////////////////////////////////////////////////////////////////
//
// Module      :  memlib.h
// Copyright   :  (c) Andy Arvanitis 2019
// License     :  BSD
//
// Maintainer  :  Andy Arvanitis
// Stability   :  experimental
// Portability :
//
// Runtime types to memory management
//
///////////////////////////////////////////////////////////////////////////////
//
#ifndef purescript_memlib_H
#define purescript_memlib_H

#if !defined(PURESCRIPT_USE_BOOST_SHARED_PTR)
#include <memory>
#else
#include <boost/shared_ptr.hpp>
#include <boost/make_shared.hpp>
#include <boost/weak_ptr.hpp>
#endif

namespace purescript {

#if !defined(PURESCRIPT_USE_BOOST_SHARED_PTR)
    namespace memlib {
        using namespace std;
    }
#else
    namespace memlib {
        using namespace boost;
    }
#endif

}

#endif // purescript_memlib_H

