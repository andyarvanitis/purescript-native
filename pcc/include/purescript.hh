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
#include <tuple>
#include <stdexcept>
#include "bind.hh"
#include "memory.hh"
#include "cmap.hh"
#include "shared_list.hh"

namespace PureScript {

template <typename A, typename B>
using fn = std::function<B(const A)>;

template <typename T>
using param = typename std::conditional<std::is_fundamental<T>::value, const T, const T&>::type;

template <typename B>
using eff_fn = std::function<B()>;

using string = std::string;
using namespace std::literals::string_literals;

template <typename... Types>
using tuple = std::tuple<Types...>;

using runtime_error = std::runtime_error;

template <typename T>
using array = shared_list<T>; // for now, at least

template <typename T>
using list = shared_list<T>;

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

#define typename(...) template <__VA_ARGS__> class

// Used for rank-N types
template <typename T>
const T typeval = T();

template <class T>
using remove_const = std::remove_const<T>;

} // namespace PureScript

#endif // PureScript_HH
