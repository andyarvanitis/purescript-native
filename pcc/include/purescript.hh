///////////////////////////////////////////////////////////////////////////////
//
// Module      :  purescript.hh
// Copyright   :  (c) Andy Arvanitis 2015
// License     :  MIT
//
// Maintainer  :  Andy Arvanitis <andy.arvanitis@gmail.com>
// Stability   :  experimental
// Portability :
//
// Basic types and functions to support purescript-to-C++1x rendering
//
///////////////////////////////////////////////////////////////////////////////
//
#ifndef PureScript_HH
#define PureScript_HH

// Standard includes
//
#include <functional>
#include <string>
#include <stdexcept>
#include "memory.hh"

namespace PureScript {

using nullptr_t = std::nullptr_t;

using string = std::string;
using namespace std::literals::string_literals;

using runtime_error = std::runtime_error;

template <class T>
using remove_const = std::remove_const<T>;

} // namespace PureScript

#endif // PureScript_HH
