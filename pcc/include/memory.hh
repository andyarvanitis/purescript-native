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
#include <functional>
#include <unordered_map>
#include <string>
#include <vector>

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

class unsafe_any {
  private:
  union {
    std::shared_ptr<void> ptr;
    int i;
    long l;
    double d;
    char c;
    // std::string s;
    std::function<unsafe_any(const unsafe_any&)> f;
    std::function<unsafe_any()> ef;
  };

  template <typename T, typename Enable = void>
  struct Helper;

  // This specialization handles cases where type T is already a (shared) pointer
  template <typename T>
  struct Helper <T, typename std::enable_if<std::is_assignable<decltype(ptr),T>::value>::type> {
    static inline auto getPtr(const T& val) -> std::shared_ptr<void> {
      return val;
    }
    static constexpr auto castPtr(const std::shared_ptr<void>& ptr) -> T {
      return std::static_pointer_cast<typename T::element_type>(ptr);
    }
  };

  // This specialization handles cases where type T is NOT a pointer
  template <typename T>
  struct Helper <T, typename std::enable_if<!std::is_assignable<decltype(ptr),T>::value && !std::is_arithmetic<T>::value>::type> {
    static inline auto getPtr(const T& val) -> std::shared_ptr<void> {
      return std::make_shared<T>(val);
    }
    static constexpr auto castPtr(const std::shared_ptr<void>& ptr) -> T {
      return *std::static_pointer_cast<T>(ptr);
    }
  };

  public:

  constexpr
  unsafe_any(const int& val) : i(val) {}
  constexpr
  unsafe_any(const long& val) : l(val) {}
  constexpr
  unsafe_any(const double& val) : d(val) {}
  constexpr
  unsafe_any(const char& val) : c(val) {}

  template <typename T>
  unsafe_any(const T& val) : ptr(Helper<T>::getPtr(val)) {}

  // unsafe_any(const char * val) : ptr(Helper<std::string>::getPtr(val)) {}

  unsafe_any(const unsafe_any& val) { ptr = val.ptr; }

  unsafe_any() = default;
  ~unsafe_any() {};


  template <typename T>
  constexpr auto cast() const -> typename std::enable_if<std::is_same<T, int>::value, T>::type {
    return i;
  }

  template <typename T>
  constexpr auto cast() const -> typename std::enable_if<std::is_same<T, double>::value, T>::type {
    return d;
  }

  template <typename T>
  constexpr auto cast() const -> typename std::enable_if<!std::is_arithmetic<T>::value, T>::type {
    return Helper<T>::castPtr(ptr);
  }

  unsafe_any operator()(unsafe_any arg) const {
    return f(arg);
  }

  unsafe_any operator()() const {
    return ef();
  }

  // TODO: long?
  //
  operator int() const {
    return i;
  }

  inline auto operator[](const std::string&) const -> unsafe_any;

  inline auto operator[](const size_t) const -> unsafe_any;

};

using any_map = std::unordered_map<std::string, const unsafe_any>;

#define make_map(...) std::make_shared<any_map>(__VA_ARGS__)

inline auto unsafe_any::operator[](const std::string& rhs) const -> unsafe_any {
  return this->cast<any_map>().at(rhs);
}

using vector = std::vector<unsafe_any>;

// TODO: list/array
//
inline auto unsafe_any::operator[](const size_t rhs) const -> unsafe_any {
  return this->cast<vector>()[rhs];
}

// TODO: use this instead?
//
using any = unsafe_any;


} // namespace PureScript

#endif // PS_memory_HH
