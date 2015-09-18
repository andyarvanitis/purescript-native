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
#include <cassert>
#include <stdexcept>

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

class any {

  public:
  enum class Type : std::int8_t { Unknown = 0, Ptr, Int, Double, Char, String, Fn, EffFn };

  const Type type = Type::Unknown;

  private:
  using fn     = std::function<any(const any&)>;
  using eff_fn = std::function<any()>;

  union {
    const std::shared_ptr<void> p;
    const long i;
    const double d;
    const char c;
    const std::string s;
    const fn f;
    const eff_fn e;
  };

  template <typename T, typename Enable = void>
  struct Helper;

  // This specialization handles cases where type T is already a (shared) pointer
  template <typename T>
  struct Helper <T, typename std::enable_if<std::is_assignable<decltype(p),T>::value>::type> {
    static inline auto getPtr(const T& val) -> std::shared_ptr<void> {
      return val;
    }
    static constexpr auto castPtr(const std::shared_ptr<void>& ptr) -> T {
      return std::static_pointer_cast<typename T::element_type>(ptr);
    }
  };

  // This specialization handles cases where type T is NOT a pointer
  template <typename T>
  struct Helper <T, typename std::enable_if<!std::is_assignable<decltype(p),T>::value && !std::is_arithmetic<T>::value>::type> {
    static inline auto getPtr(const T& val) -> std::shared_ptr<void> {
      return std::make_shared<T>(val);
    }
    static constexpr auto castPtr(const std::shared_ptr<void>& ptr) -> T {
      return *std::static_pointer_cast<T>(ptr);
    }
  };

  public:

  constexpr
  any(const int val) : type(Type::Int), i(val) {}

  constexpr
  any(const long val) : type(Type::Int), i(val) {}

  constexpr
  any(const double val) : type(Type::Double), d(val) {}

  constexpr
  any(const char val) : type(Type::Char), c(val) {}

  any(const std::string& val) : type(Type::String), s(val) {}
  any(const char* const val) : type(Type::String), s(val) {}

  any(const fn& val) : type(Type::Fn), f(val) {}
  any(const eff_fn& val) : type(Type::EffFn), e(val) {}

  template <typename T>
  any(const T& val) : p(Helper<T>::getPtr(val)) {}

  // any(const char * val) : p(Helper<std::string>::getPtr(val)) {}

  any(const any& val) {
    assert("copy constructor called");
  }

  any() = default;
  ~any() {};


  template <typename T>
  constexpr auto cast() const -> typename std::enable_if<std::is_same<T, long>::value, T>::type {
    assert(type == Type::Int);
    return i;
  }

  template <typename T>
  constexpr auto cast() const -> typename std::enable_if<std::is_same<T, double>::value, T>::type {
    assert(type == Type::Double);
    return d;
  }

  template <typename T>
  constexpr auto cast() const -> typename std::enable_if<std::is_same<T, char>::value, T>::type {
    assert(type == Type::Char);
    return c;
  }

  template <typename T>
  constexpr auto cast() const -> typename std::enable_if<!std::is_arithmetic<T>::value, T>::type {
    assert(type == Type::Ptr);
    return Helper<T>::castPtr(p);
  }

  any operator()(any arg) const {
    assert(type == Type::Fn);
    return f(arg);
  }

  any operator()() const {
    assert(type == Type::EffFn);
    return e();
  }

  // operator int() const {
  //   return i;
  // }

  inline auto operator[](const std::string&) const -> any;

  inline auto operator[](const size_t) const -> any;

  inline auto operator==(const any& rhs) const -> bool {
    switch (type) {
      case Type::Int:     return i == rhs.i;
      case Type::Double:  return d == rhs.d;
      case Type::Char:    return c == rhs.c;
      case Type::String:  return s == rhs.s;
      case Type::Ptr:     return p == rhs.p;
      default: throw std::runtime_error("unsupported type");
    }
  }

  inline auto operator<(const any& rhs) const -> bool {
    switch (type) {
      case Type::Int:     return i < rhs.i;
      case Type::Double:  return d < rhs.d;
      case Type::Char:    return c < rhs.c;
      case Type::String:  return s < rhs.s;
      default: throw std::runtime_error("unsupported type");
    }
  }

  inline auto operator>(const any& rhs) const -> bool {
    switch (type) {
      case Type::Int:     return i > rhs.i;
      case Type::Double:  return d > rhs.d;
      case Type::Char:    return c > rhs.c;
      case Type::String:  return s > rhs.s;
      default: throw std::runtime_error("unsupported type");
    }
  }

  inline auto operator+(const any& rhs) const -> any {
    switch (type) {
      case Type::Int:     return i + rhs.i;
      case Type::Double:  return d + rhs.d;
      case Type::Char:    return c + rhs.c;
      case Type::String:  return s + rhs.s;
      default: throw std::runtime_error("unsupported type");
    }
  }

  inline auto operator-(const any& rhs) const -> any {
    switch (type) {
      case Type::Int:     return i - rhs.i;
      case Type::Double:  return d - rhs.d;
      case Type::Char:    return c - rhs.c;
      default: throw std::runtime_error("unsupported type");
    }
  }

  inline auto operator*(const any& rhs) const -> any {
    switch (type) {
      case Type::Int:     return i * rhs.i;
      case Type::Double:  return d * rhs.d;
      default: throw std::runtime_error("unsupported type");
    }
  }

  inline auto operator/(const any& rhs) const -> any {
    switch (type) {
      case Type::Int:     return i / rhs.i;
      case Type::Double:  return d / rhs.d;
      default: throw std::runtime_error("unsupported type");
    }
  }

  inline auto operator%(const any& rhs) const -> any {
    switch (type) {
      case Type::Int:     return i % rhs.i;
      default: throw std::runtime_error("unsupported type");
    }
  }

};

using any_map = std::unordered_map<std::string, const any>;

#define make_map(...) std::make_shared<any_map>(__VA_ARGS__)

inline auto any::operator[](const std::string& rhs) const -> any {
  return this->cast<any_map>().at(rhs);
}

using vector = std::vector<any>;

// TODO: list/array
//
inline auto any::operator[](const size_t rhs) const -> any {
  return this->cast<vector>()[rhs];
}


} // namespace PureScript

#endif // PS_memory_HH
