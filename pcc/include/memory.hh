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
#include <iostream>

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

  // template <typename T, typename U>
  // constexpr auto cast(const std::shared_ptr<U>& a) -> typename T::element_type {
  //   return *std::dynamic_pointer_cast<typename T::element_type>(a);
  // }
  //
  // template <typename U>
  // constexpr auto cast(const std::shared_ptr<U>& a) -> U {
  //   return *std::dynamic_pointer_cast<U>(a);
  // }
}

// template <typename B, typename A>
// constexpr auto instance_of(A a) {
//   return Private::instance_of<B>(a);
// }

// template <typename B, typename A>
// constexpr auto cast(A a) {
//   return Private::cast<B>(a);
// }

// Note type transformation A<Args> -> B<...> -> B<Args>

// template <template <typename...> class B, template <typename...> class A, typename... Args>
// constexpr auto instance_of(const std::shared_ptr<A<Args...>>& a) {
//   return Private::instance_of<B<Args...>>(a);
// }

// template <template <typename...> class B, template <typename...> class A, typename... Args>
// constexpr auto cast(const std::shared_ptr<A<Args...>>& a) {
//   return Private::cast<B<Args...>>(a);
// }

class any {

  public:
  enum class Type : std::int8_t {
    Unknown,
    Pointer,
    Integer,
    Double,
    Character,
    Boolean,
    String,
    Map,
    Vector,
    Function,
    EffectFunction
  };

  const Type type = Type::Unknown;

  using string = std::string;
  using fn     = std::function<any(const any&)>;
  using eff_fn = std::function<any()>;
  using map    = std::unordered_map<string, const any>;
  using vector = std::vector<any>;

  private:
  union {
    mutable managed<void> p;
    mutable long   i;
    mutable double d;
    mutable char   c;
    mutable bool   b;
    mutable string s;
    mutable map    m;
    mutable vector v;
    mutable fn     f;
    mutable eff_fn e;
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
  struct Helper <T, typename std::enable_if<!std::is_assignable<decltype(p),T>::value>::type> {
    static inline auto getPtr(const T& val) -> std::shared_ptr<void> {
      return std::make_shared<T>(val);
    }
    static constexpr auto castPtr(const std::shared_ptr<void>& ptr) -> T {
      return *std::static_pointer_cast<T>(ptr);
    }
  };

  public:

  constexpr
  any(const int val) : type(Type::Integer), i(val) {}

  constexpr
  any(const long val) : type(Type::Integer), i(val) {}

  constexpr
  any(const double val) : type(Type::Double), d(val) {}

  constexpr
  any(const char val) : type(Type::Character), c(val) {}

  constexpr
  any(const bool val) : type(Type::Boolean), b(val) {}

  any(const string& val) : type(Type::String), s(val) {}
  any(const char* const val) : type(Type::String), s(val) {}

  any(const map& val) : type(Type::Map), m(val) {}

  any(const vector& val) : type(Type::Vector), v(val) {}

  template <typename T>
  any(const T& val, typename std::enable_if<std::is_assignable<fn,T>::value>::type* = 0)
    : type(Type::Function), f(val) {}

  template <typename T>
  any(const T& val, typename std::enable_if<std::is_assignable<eff_fn,T>::value>::type* = 0)
    : type(Type::EffectFunction), e(val) {}

  template <typename T>
  any(const T& val, typename std::enable_if<std::is_assignable<managed<void>,T>::value>::type* = 0)
    : type(Type::Pointer), p(Helper<T>::getPtr(val)) {}

  const any& operator=(const any& val) const {
    assert(false);
    *this = val;
    return *this;
  }

  any(const any& val) : type(val.type) {
    std::cout << "copy" << std::endl;
    switch (type) {
      case Type::Integer:        i = val.i;  break;
      case Type::Double:         d = val.d;  break;
      case Type::Character:      c = val.c;  break;
      case Type::Boolean:        d = val.b;  break;
      case Type::String:         s = val.s;  break;
      case Type::Map:            m = val.m;  break;
      case Type::Vector:         v = val.v;  break;
      case Type::Function:       f = val.f;  break;
      case Type::EffectFunction: e = val.e;  break;
      case Type::Pointer:        p = val.p;  break;
      default: throw std::runtime_error("unsupported type in copy ctor");
    }
  }

  any(any&& val) : type(val.type) {
    std::cout << "move " << std::endl;
    switch (type) {
      case Type::Integer:        i = std::move(val.i);  break;
      case Type::Double:         d = std::move(val.d);  break;
      case Type::Character:      c = std::move(val.c);  break;
      case Type::Boolean:        d = std::move(val.b);  break;
      case Type::String:         s = std::move(val.s);  break;
      case Type::Map:            m = std::move(val.m);  break;
      case Type::Vector:         v = std::move(val.v);  break;
      case Type::Function:       f = std::move(val.f);  break;
      case Type::EffectFunction: e = std::move(val.e);  break;
      case Type::Pointer:        p = std::move(val.p);  break;
      default: throw std::runtime_error("unsupported type in move ctor");
    }
  }

  any() = delete;
  ~any() {};

  template <typename T>
  constexpr auto cast() const -> typename std::enable_if<std::is_same<T, long>::value, T>::type {
    assert(type == Type::Integer);
    return i;
  }

  template <typename T>
  constexpr auto cast() const -> typename std::enable_if<std::is_same<T, double>::value, T>::type {
    assert(type == Type::Double);
    return d;
  }

  template <typename T>
  constexpr auto cast() const -> typename std::enable_if<std::is_same<T, char>::value, T>::type {
    assert(type == Type::Character);
    return c;
  }

  template <typename T>
  constexpr auto cast() const -> typename std::enable_if<std::is_same<T, bool>::value, T>::type {
    assert(type == Type::Boolean);
    return b;
  }

  template <typename T>
  constexpr auto cast() const -> typename std::enable_if<std::is_same<T, string>::value, const T&>::type {
    std::cout << int(type) << std::endl;
    assert(type == Type::String);
    return s;
  }

  template <typename T>
  constexpr auto cast() const -> typename std::enable_if<std::is_same<T, map>::value, const T&>::type {
    assert(type == Type::Map);
    return m;
  }

  template <typename T>
  constexpr auto cast() const -> typename std::enable_if<std::is_same<T, vector>::value, const T&>::type {
    assert(type == Type::Vector);
    return v;
  }

  template <typename T>
  constexpr auto cast() const ->
      typename std::enable_if<std::is_assignable<std::shared_ptr<void>,T>::value, const T>::type {
    assert(type == Type::Pointer);
    return Helper<T>::castPtr(p);
  }

  auto operator()(const any arg) const -> any {
    assert(type == Type::Function);
    return f(arg);
  }

  auto operator()() const -> any {
    assert(type == Type::EffectFunction);
    return e();
  }

  // operator long() const {
  //   return i;
  // }
  //
  // operator double() const {
  //   return d;
  // }
  //
  // operator bool() const {
  //   return b;
  // }

  operator string&() const {
    return s;
  }

  operator map&() const {
    return m;
  }

  operator vector&() const {
    return v;
  }

  template <typename T>
  inline auto instance_of() const -> bool {
    return Private::instance_of<T>(p);
  }

  inline auto operator[](const string& rhs) const -> const any& {
    assert(type == Type::Map);
    return m.at(rhs);
  }

  inline auto operator[](const size_t rhs) const -> const any& {
    assert(type == Type::Vector);
    return v[rhs];
  }

  inline auto operator==(const any& rhs) const -> bool {
    switch (type) {
      case Type::Integer:   return i == rhs.i;
      case Type::Double:    return d == rhs.d;
      case Type::Character: return c == rhs.c;
      case Type::Boolean:   return b == rhs.b;
      case Type::String:    return s == rhs.s;
      case Type::Pointer:   return p == rhs.p;
      default: throw std::runtime_error("unsupported type for '==' operator");
    }
  }

  inline auto operator!=(const any& rhs) const -> bool {
    switch (type) {
      case Type::Integer:   return i != rhs.i;
      case Type::Double:    return d != rhs.d;
      case Type::Character: return c != rhs.c;
      case Type::Boolean:   return b != rhs.b;
      case Type::String:    return s != rhs.s;
      case Type::Pointer:   return p != rhs.p;
      default: throw std::runtime_error("unsupported type for '!=' operator");
    }
  }

  inline auto operator<(const any& rhs) const -> bool {
    switch (type) {
      case Type::Integer:   return i < rhs.i;
      case Type::Double:    return d < rhs.d;
      case Type::Character: return c < rhs.c;
      case Type::String:    return s < rhs.s;
      default: throw std::runtime_error("unsupported type for '<' operator");
    }
  }

  inline auto operator>(const any& rhs) const -> bool {
    switch (type) {
      case Type::Integer:   return i > rhs.i;
      case Type::Double:    return d > rhs.d;
      case Type::Character: return c > rhs.c;
      case Type::String:    return s > rhs.s;
      default: throw std::runtime_error("unsupported type for '>' operator");
    }
  }

  inline auto operator+(const any& rhs) const -> any {
    switch (type) {
      case Type::Integer:   return i + rhs.i;
      case Type::Double:    return d + rhs.d;
      case Type::Character: return c + rhs.c;
      case Type::String:    return s + rhs.s;
      default: throw std::runtime_error("unsupported for '+' operator");
    }
  }

  inline auto operator-(const any& rhs) const -> any {
    switch (type) {
      case Type::Integer:   return i - rhs.i;
      case Type::Double:    return d - rhs.d;
      case Type::Character: return c - rhs.c;
      default: throw std::runtime_error("unsupported type for '-' binary operator");
    }
  }

  inline auto operator*(const any& rhs) const -> any {
    switch (type) {
      case Type::Integer: return i * rhs.i;
      case Type::Double:  return d * rhs.d;
      default: throw std::runtime_error("unsupported type for '*' operator");
    }
  }

  inline auto operator/(const any& rhs) const -> any {
    switch (type) {
      case Type::Integer: return i / rhs.i;
      case Type::Double:  return d / rhs.d;
      default: throw std::runtime_error("unsupported type for '/' operator");
    }
  }

  inline auto operator%(const any& rhs) const -> any {
    switch (type) {
      case Type::Integer: return i % rhs.i;
      default: throw std::runtime_error("unsupported type for '%' operator");
    }
  }
};

template <typename T>
constexpr auto cast(const any& a) {
  return *(a.cast<T>());
}

} // namespace PureScript

#endif // PS_memory_HH
