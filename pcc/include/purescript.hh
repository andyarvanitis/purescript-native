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
#include "bind.hh"
#include "memory.hh"
#include "any_map.hh"
#include "shared_list.hh"

namespace PureScript {

template <typename A, typename B>
using fn = std::function<B(A)>;

template <typename B>
using eff_fn = std::function<B()>;

using string = std::string;

using runtime_error = std::runtime_error;

template <typename T>
using list = shared_list<T>;

using list_index_type = list<void*>::size_type;

// Support for things like ((->) r)
template <typename R>
struct fn_ {
  template <typename T>
  using _ = fn<R,T>;
};

// Used by type constructor templates
template <typename T>
using void1 = void;
template <typename T1, typename T2>
using void2 = void;
template <typename T1, typename T2, typename T3>
using void3 = void;
template <typename T1, typename T2, typename T3, typename T4>
using void4 = void;
template <typename T1, typename T2, typename T3, typename T4, typename T5>
using void5 = void;

} // namespace PureScript

#endif // PureScript_HH
