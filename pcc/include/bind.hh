///////////////////////////////////////////////////////////////////////////////
//
// Module      :  bind.hh
// Copyright   :  (c) Andy Arvanitis 2015
// License     :  MIT
//
// Maintainer  :  Andy Arvanitis <andy.arvanitis@gmail.com>
// Stability   :  experimental
// Portability :
//
// Types, macros, and functions to support function partial application
//
///////////////////////////////////////////////////////////////////////////////
//
#ifndef PS_bind_HH
#define PS_bind_HH

#include <functional>

namespace PureScript {

namespace Private {
  template <int N>
  struct Bind {
  };
}

template <int N, typename... Args>
constexpr auto bind(Args&&... args) -> decltype(Private::Bind<N>::bind(std::forward<Args>(args)...)) {
  return Private::Bind<N>::bind(std::forward<Args>(args)...);
}

#define BIND_WITH_PLACEHOLDERS(N, ...) \
namespace Private { \
  using namespace std::placeholders; \
  template <> \
  struct Bind<N> { \
    template <typename... Args> \
    static constexpr auto bind(Args&&... args) -> decltype(std::bind(std::forward<Args>(args)..., __VA_ARGS__)) { \
      return std::bind(std::forward<Args>(args)..., __VA_ARGS__); \
    } \
  }; \
}

BIND_WITH_PLACEHOLDERS( 1, _1)
BIND_WITH_PLACEHOLDERS( 2, _1, _2)
BIND_WITH_PLACEHOLDERS( 3, _1, _2, _3)
BIND_WITH_PLACEHOLDERS( 4, _1, _2, _3, _4)
BIND_WITH_PLACEHOLDERS( 5, _1, _2, _3, _4, _5)
BIND_WITH_PLACEHOLDERS( 6, _1, _2, _3, _4, _5, _6)
BIND_WITH_PLACEHOLDERS( 7, _1, _2, _3, _4, _5, _6, _7)
BIND_WITH_PLACEHOLDERS( 8, _1, _2, _3, _4, _5, _6, _7, _8)
BIND_WITH_PLACEHOLDERS( 9, _1, _2, _3, _4, _5, _6, _7, _8, _9)
BIND_WITH_PLACEHOLDERS(10, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10)

#undef BIND_WITH_PLACEHOLDERS

} // namespace PureScript

#endif // PS_bind_HH
