///////////////////////////////////////////////////////////////////////////////
//
// Module      :  memory.hh
// Copyright   :  (c) Andy Arvanitis 2015
// License     :  MIT
//
// Maintainer  :  Andy Arvanitis <andy.arvanitis@gmail.com>
// Stability   :  experimental
// Portability :
//
// Support for reference-count-based memory management
//
///////////////////////////////////////////////////////////////////////////////
//
#ifndef PS_memory_HH
#define PS_memory_HH

#include <memory>
#include "bind.hh"

namespace PureScript {

template <typename T>
using managed = std::shared_ptr<T>;

template <typename T, typename... ArgTypes>
auto construct(ArgTypes... args) ->
    typename std::enable_if<std::is_assignable<std::shared_ptr<void>,T>::value,T>::type {
  return std::make_shared<typename T::element_type>(args...);
}

template <typename T, typename... ArgTypes>
constexpr auto construct(ArgTypes... args) ->
    typename std::enable_if<!std::is_assignable<std::shared_ptr<void>,T>::value,std::shared_ptr<T>>::type {
  return std::make_shared<T>(args...);
}

template <typename Ctor, typename... CArgs, typename... Args>
constexpr auto constructor(Args&&... args) ->
    decltype(Private::Bind<sizeof...(CArgs) - sizeof...(Args)>::bind(construct<Ctor, CArgs...>,std::forward<Args>(args)...)) {
  return Private::Bind<sizeof...(CArgs) - sizeof...(Args)>::bind(construct<Ctor, CArgs...>, std::forward<Args>(args)...);
}

namespace Private {
  template <typename T, typename U>
  constexpr auto instance_of(const std::shared_ptr<U>& a) ->
      typename std::enable_if<std::is_assignable<std::shared_ptr<void>,T>::value,T>::type {
    return std::dynamic_pointer_cast<typename T::element_type>(a);
  }

  template <typename T, typename U>
  constexpr auto instance_of(const std::shared_ptr<U>& a) ->
    typename std::enable_if<!std::is_assignable<std::shared_ptr<void>,T>::value,std::shared_ptr<T>>::type {
    return std::dynamic_pointer_cast<T>(a);
  }

  template <typename T, typename U>
  constexpr auto cast(const std::shared_ptr<U>& a) -> typename T::element_type {
    return *std::dynamic_pointer_cast<typename T::element_type>(a);
  }

  template <typename U>
  constexpr auto cast(const std::shared_ptr<U>& a) -> U {
    return *std::dynamic_pointer_cast<U>(a);
  }
}

template <typename B, typename A>
constexpr auto instance_of(A a) {
  return Private::instance_of<B>(a);
}

template <typename B, typename A>
constexpr auto cast(A a) {
  return Private::cast<B>(a);
}

// Note type transformation A<Args> -> B<...> -> B<Args>

template <template <typename...> class B, template <typename...> class A, typename... Args>
constexpr auto instance_of(const std::shared_ptr<A<Args...>>& a) {
  return Private::instance_of<B<Args...>>(a);
}

template <template <typename...> class B, template <typename...> class A, typename... Args>
constexpr auto cast(const std::shared_ptr<A<Args...>>& a) {
  return Private::cast<B<Args...>>(a);
}

} // namespace PureScript

#endif // PS_memory_HH
