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

template <typename T, typename U>
constexpr auto instance_of(const std::shared_ptr<U>& a) ->
    typename std::enable_if<std::is_assignable<std::shared_ptr<void>,T>::value,T>::type {
  return std::dynamic_pointer_cast<typename T::element_type>(a);
}

template <typename T, typename U>
constexpr auto instance_of(const std::shared_ptr<U> a) ->
  typename std::enable_if<!std::is_assignable<std::shared_ptr<void>,T>::value,std::shared_ptr<T>>::type {
  return std::dynamic_pointer_cast<T>(a);
}

template <typename T, typename U>
constexpr auto get(const std::shared_ptr<U>& a) -> typename T::element_type {
  return *std::dynamic_pointer_cast<typename T::element_type>(a);
}

template <typename U>
constexpr auto get(const std::shared_ptr<U>& a) -> U {
  return *std::dynamic_pointer_cast<U>(a);
}

class unsafe_any {
  private:
  std::shared_ptr<void> ptr;

  template <typename T, typename Enable = void>
  struct Helper;

  // This specialization handles cases where type T is already a (shared) pointer
  template <typename T>
  struct Helper <T, typename std::enable_if<std::is_assignable<decltype(ptr),T>::value>::type> {
    static inline auto getPtr(const T& val) -> std::shared_ptr<void> {
      return val;
    }
    static inline auto castPtr(const std::shared_ptr<void>& ptr) -> T {
      return std::static_pointer_cast<typename T::element_type>(ptr);
    }
  };

  // This specialization handles cases where type T is NOT a pointer
  template <typename T>
  struct Helper <T, typename std::enable_if<!std::is_assignable<decltype(ptr),T>::value>::type> {
    static inline auto getPtr(const T& val) -> std::shared_ptr<void> {
      return std::make_shared<T>(val);
    }
    static inline auto castPtr(const std::shared_ptr<void>& ptr) -> T {
      return *std::static_pointer_cast<T>(ptr);
    }
  };

  public:
  template <typename T>
  unsafe_any(const T& val) : ptr(Helper<T>::getPtr(val)) {}
  unsafe_any(const char * val) : ptr(Helper<std::string>::getPtr(val)) {}

  unsafe_any() = default;
  ~unsafe_any() = default;

  template <typename T>
  inline auto cast() const -> T {
    return Helper<T>::castPtr(ptr);
  }
};

} // namespace PureScript

#endif // PS_memory_HH
