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
    const managed<void> p;
    const long   i;
    const double d;
    const char   c;
    const bool   b;
    const string s;
    const map    m;
    const vector v;
    const fn     f;
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

  private:
  inline auto copy(const any& val) const {
    const_cast<Type&>(type) = val.type;
    switch (type) {
      case Type::Integer:        const_cast<long&>(i)          = val.i;  break;
      case Type::Double:         const_cast<double&>(d)        = val.d;  break;
      case Type::Character:      const_cast<char&>(c)          = val.c;  break;
      case Type::Boolean:        const_cast<bool&>(b)          = val.b;  break;
      case Type::String:         const_cast<string&>(s)        = val.s;  break;
      case Type::Map:            const_cast<map&>(m)           = val.m;  break;
      case Type::Vector:         const_cast<vector&>(v)        = val.v;  break;
      case Type::Function:       const_cast<fn&>(f)            = val.f;  break;
      case Type::EffectFunction: const_cast<eff_fn&>(e)        = val.e;  break;
      case Type::Pointer:        const_cast<managed<void>&>(p) = val.p;  break;
      default: throw std::runtime_error("unsupported type in copy ctor");
    }
  }

  public:
  inline const any& operator=(const any& val) const {
    copy(val);
    return *this;
  }
  any(const any& val) {
    copy(val);
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
  constexpr auto cast() const -> typename std::enable_if<std::is_same<T, map>::value, T>::type {
    assert(type == Type::Map);
    return m;
  }

  template <typename T>
  constexpr auto cast() const -> typename std::enable_if<std::is_same<T, vector>::value, T>::type {
    assert(type == Type::Vector);
    return v;
  }

  template <typename T>
  constexpr auto cast() const -> typename std::enable_if<std::is_same<T, string>::value, T>::type {
    std::cout << int(type) << std::endl;
    assert(type == Type::String);
    return s;
  }

  template <typename T>
  constexpr auto cast() const ->
      typename std::enable_if<std::is_assignable<std::shared_ptr<void>,T>::value, T>::type {
    assert(type == Type::Pointer);
    return Helper<T>::castPtr(p);
  }

  any operator()(any arg) const {
    assert(type == Type::Function);
    return f(arg);
  }

  any operator()() const {
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

  template <typename T>
  inline auto instance_of() const -> bool {
    return Private::instance_of<T>(p);
  }

  inline auto operator[](const string& rhs) const -> any {
    assert(type == Type::Map);
    return m.at(rhs);
  }

  inline auto operator[](const size_t rhs) const -> any {
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
