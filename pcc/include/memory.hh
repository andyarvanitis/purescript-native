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
    EffFunction,
    Thunk
  };

  const Type type = Type::Unknown;

  using string = std::string;
  using map    = std::unordered_map<string, const any>;
  using vector = std::vector<any>;
  using fn     = std::function<any(const any&)>;
  using eff_fn = std::function<any()>;
  using thunk  = std::function<const any& (bool, bool)>;

  private:
  union {
    mutable std::shared_ptr<void> p;
    mutable long   i;
    mutable double d;
    mutable char   c;
    mutable bool   b;
    mutable string s;
    mutable map    m;
    mutable vector v;
    mutable fn     f;
    mutable eff_fn e;
    mutable thunk  t;
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
  any(string&& val) : type(Type::String), s(std::move(val)) {}

  any(const map& val) : type(Type::Map), m(val) {}
  any(map&& val) : type(Type::Map), m(std::move(val)) {}

  any(const vector& val) : type(Type::Vector), v(val) {}
  any(vector&& val) : type(Type::Vector), v(std::move(val)) {}

  template <typename T>
  any(const T& val, typename std::enable_if<std::is_assignable<fn,T>::value>::type* = 0)
    : type(Type::Function), f(val) {}

  // template <typename T>
  // any(T&& val, typename std::enable_if<std::is_assignable<fn,T>::value>::type* = 0)
  //   : type(Type::Function), f(std::move(val)) {}

  template <typename T>
  any(const T& val, typename std::enable_if<std::is_assignable<eff_fn,T>::value>::type* = 0)
    : type(Type::EffFunction), e(val) {}

  template <typename T>
  any(const T& val, typename std::enable_if<std::is_assignable<thunk,T>::value>::type* = 0)
    : type(Type::Thunk), t(val) {}

  template <typename T>
  any(const T& val, typename std::enable_if<std::is_assignable<std::shared_ptr<void>,T>::value>::type* = 0)
    : type(Type::Pointer), p(val) {}

  any(std::nullptr_t) : type(Type::Pointer), p(nullptr) {}

  template <typename T>
  any(T&& val, typename std::enable_if<std::is_assignable<std::shared_ptr<void>,T>::value>::type* = 0)
    : type(Type::Pointer), p(std::move(val)) {}

  const any& operator=(const any& val) const {
    assert(false);
    *this = val;
    return *this;
  }

  any(const any& val) : type(val.type) {
    // std::cout << "copy" << std::endl;
    switch (type) {
      case Type::Integer:         i = val.i;                       break;
      case Type::Double:          d = val.d;                       break;
      case Type::Character:       c = val.c;                       break;
      case Type::Boolean:         d = val.b;                       break;
      case Type::String:          new (&s) string        (val.s);  break;
      case Type::Map:             new (&m) map           (val.m);  break;
      case Type::Vector:          new (&v) vector        (val.v);  break;
      case Type::Function:        new (&f) fn            (val.f);  break;
      case Type::EffFunction:     new (&e) eff_fn        (val.e);  break;
      case Type::Thunk:           new (&t) thunk         (val.t);  break;
      case Type::Pointer:         new (&p) std::shared_ptr<void> (val.p);  break;

      default: throw std::runtime_error("unsupported type in copy ctor");
    }
  }

  any(any&& val) : type(val.type) {
    // std::cout << "move" << std::endl;
    switch (type) {
      case Type::Integer:         i = val.i;                                  break;
      case Type::Double:          d = val.d;                                  break;
      case Type::Character:       c = val.c;                                  break;
      case Type::Boolean:         d = val.b;                                  break;
      case Type::String:          new (&s) string        (std::move(val.s));  break;
      case Type::Map:             new (&m) map           (std::move(val.m));  break;
      case Type::Vector:          new (&v) vector        (std::move(val.v));  break;
      case Type::Function:        new (&f) fn            (std::move(val.f));  break;
      case Type::EffFunction:     new (&e) eff_fn        (std::move(val.e));  break;
      case Type::Thunk:           new (&t) thunk         (std::move(val.t));  break;
      case Type::Pointer:         new (&p) std::shared_ptr<void> (std::move(val.p));  break;

      default: throw std::runtime_error("unsupported type in move ctor");
    }
  }

  any() = delete;
  ~any() {
    // std::cout << "destroy" << std::endl;
    switch (type) {
      case Type::Integer:         ;                    break;
      case Type::Double:          ;                    break;
      case Type::Character:       ;                    break;
      case Type::Boolean:         ;                    break;
      case Type::String:          s.~string();         break;
      case Type::Map:             m.~map();            break;
      case Type::Vector:          v.~vector();         break;
      case Type::Function:        f.~fn();             break;
      case Type::EffFunction:     e.~eff_fn();         break;
      case Type::Thunk:           t.~thunk();          break;
      case Type::Pointer:         p.~managed<void>();  break;

      default: throw std::runtime_error("unsupported type in destructor");
    }
  };

  #define returnValue(T, V, F) \
    if (type == T) { \
      return F(V); \
    } else { \
      if (type != Type::Thunk) std::cout << int(type) << std::endl; \
      assert(type == Type::Thunk); \
      const any& value = t(false,false); \
      assert(value.type == T); \
      return F(value.V); \
    }

  template <typename T>
  constexpr auto cast() const -> typename std::enable_if<std::is_same<T, long>::value, T>::type {
    returnValue(Type::Integer, i,)
  }

  template <typename T>
  constexpr auto cast() const -> typename std::enable_if<std::is_same<T, double>::value, T>::type {
    returnValue(Type::Double, d,)
  }

  template <typename T>
  constexpr auto cast() const -> typename std::enable_if<std::is_same<T, char>::value, T>::type {
    returnValue(Type::Character, c,)
  }

  template <typename T>
  constexpr auto cast() const -> typename std::enable_if<std::is_same<T, bool>::value, T>::type {
    returnValue(Type::Boolean, b,)
  }

  template <typename T>
  constexpr auto cast() const -> typename std::enable_if<std::is_same<T, string>::value, const T>::type {
    returnValue(Type::String, s,)
  }

  template <typename T>
  constexpr auto cast() const -> typename std::enable_if<std::is_same<T, map>::value, const T>::type {
    returnValue(Type::Map, m,)
  }

  template <typename T>
  constexpr auto cast() const -> typename std::enable_if<std::is_same<T, vector>::value, const T>::type {
    returnValue(Type::Vector, v,)
  }

  template <typename T>
  constexpr auto cast() const ->
      typename std::enable_if<std::is_assignable<std::shared_ptr<void>,T>::value, const T>::type {
    returnValue(Type::Pointer, p, std::static_pointer_cast<typename T::element_type>)
  }

  auto operator()(const any arg) const -> any {
    returnValue(Type::Function, f(arg),)
  }

  inline static auto call(const any& a) -> any {
    assert(a.type == Type::EffFunction || a.type == Type::Function);
    if (a.type == Type::EffFunction) {
      return a.e();
    } else {
      return a.f(false);
    }
  }

  auto operator()() const -> any {
    if (type == Type::EffFunction || type == Type::Function) {
      return call(*this);
    } else {
      assert(type == Type::Thunk);
      const any& value = t(false,false);
      return call(value);
    }
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

  operator const string() const {
    returnValue(Type::String, s,)
  }

  operator const map() const {
    returnValue(Type::Map, m,)
  }

  operator const vector() const {
    returnValue(Type::Vector, v,)
  }

  auto extractPointer() const -> const void* {
    returnValue(Type::Pointer, p.get(),)
  }

  template <typename T>
  auto instance_of() const ->
      typename std::enable_if<std::is_assignable<std::shared_ptr<void>,T>::value, bool>::type {
    using elem_type = const typename T::element_type;
    using base_type = const typename elem_type::base_type;
    const auto ptr = extractPointer();
    return ptr ? dynamic_cast<elem_type*>(static_cast<base_type*>(ptr)) != nullptr : false;
  }

  template <typename T>
  auto instance_of() const ->
      typename std::enable_if<!std::is_assignable<std::shared_ptr<void>,T>::value, bool>::type {
    using base_type = const typename T::base_type;
    const auto ptr = extractPointer();
    return ptr ? dynamic_cast<T*>(static_cast<base_type*>(ptr)) != nullptr : false;
  }

  inline auto operator[](const string& rhs) const -> const any {
    returnValue(Type::Map, m.at(rhs),)
  }

  inline auto operator[](const size_t rhs) const -> const any {
    returnValue(Type::Vector, v[rhs],)
  }

  inline static auto extractValue(const any& a) -> const any {
    if (a.type != Type::Thunk) {
      return a;
    } else {
      assert(a.type == Type::Thunk);
      return a.t(false,false);
    }
  }

  inline auto operator==(const any& rhs_) const -> bool {
    auto& lhs = extractValue(*this);
    auto& rhs = extractValue(rhs_);
    assert(lhs.type == rhs.type);
    switch (lhs.type) {
      case Type::Integer:   return lhs.i == rhs.i;
      case Type::Double:    return lhs.d == rhs.d;
      case Type::Character: return lhs.c == rhs.c;
      case Type::Boolean:   return lhs.b == rhs.b;
      case Type::String:    return lhs.s == rhs.s;
      case Type::Pointer:   return lhs.p == rhs.p;
      default: throw std::runtime_error("unsupported type for '==' operator");
    }
  }

  inline auto operator!=(const any& rhs_) const -> bool {
    auto& lhs = extractValue(*this);
    auto& rhs = extractValue(rhs_);
    assert(lhs.type == rhs.type);
    switch (lhs.type) {
      case Type::Integer:   return lhs.i != rhs.i;
      case Type::Double:    return lhs.d != rhs.d;
      case Type::Character: return lhs.c != rhs.c;
      case Type::Boolean:   return lhs.b != rhs.b;
      case Type::String:    return lhs.s != rhs.s;
      case Type::Pointer:   return lhs.p != rhs.p;
      default: throw std::runtime_error("unsupported type for '!=' operator");
    }
  }

  inline auto operator<(const any& rhs_) const -> bool {
    auto& lhs = extractValue(*this);
    auto& rhs = extractValue(rhs_);
    assert(lhs.type == rhs.type);
    switch (lhs.type) {
      case Type::Integer:   return lhs.i < rhs.i;
      case Type::Double:    return lhs.d < rhs.d;
      case Type::Character: return lhs.c < rhs.c;
      case Type::String:    return lhs.s < rhs.s;
      default: throw std::runtime_error("unsupported type for '<' operator");
    }
  }

  inline auto operator>(const any& rhs_) const -> bool {
    auto& lhs = extractValue(*this);
    auto& rhs = extractValue(rhs_);
    assert(lhs.type == rhs.type);
    switch (lhs.type) {
      case Type::Integer:   return lhs.i > rhs.i;
      case Type::Double:    return lhs.d > rhs.d;
      case Type::Character: return lhs.c > rhs.c;
      case Type::String:    return lhs.s > rhs.s;
      default: throw std::runtime_error("unsupported type for '>' operator");
    }
  }

  inline auto operator+(const any& rhs_) const -> any {
    auto& lhs = extractValue(*this);
    auto& rhs = extractValue(rhs_);
    assert(lhs.type == rhs.type);
    switch (lhs.type) {
      case Type::Integer:   return lhs.i + rhs.i;
      case Type::Double:    return lhs.d + rhs.d;
      case Type::Character: return lhs.c + rhs.c;
      case Type::String:    return lhs.s + rhs.s;
      default: throw std::runtime_error("unsupported for '+' operator");
    }
  }

  inline auto operator-(const any& rhs_) const -> any {
    auto& lhs = extractValue(*this);
    auto& rhs = extractValue(rhs_);
    assert(lhs.type == rhs.type);
    switch (lhs.type) {
      case Type::Integer:   return lhs.i - rhs.i;
      case Type::Double:    return lhs.d - rhs.d;
      case Type::Character: return lhs.c - rhs.c;
      default: throw std::runtime_error("unsupported type for '-' binary operator");
    }
  }

  inline auto operator*(const any& rhs_) const -> any {
    auto& lhs = extractValue(*this);
    auto& rhs = extractValue(rhs_);
    assert(lhs.type == rhs.type);
    switch (lhs.type) {
      case Type::Integer: return lhs.i * rhs.i;
      case Type::Double:  return lhs.d * rhs.d;
      default: throw std::runtime_error("unsupported type for '*' operator");
    }
  }

  inline auto operator/(const any& rhs_) const -> any {
    auto& lhs = extractValue(*this);
    auto& rhs = extractValue(rhs_);
    assert(lhs.type == rhs.type);
    switch (lhs.type) {
      case Type::Integer: return lhs.i / rhs.i;
      case Type::Double:  return lhs.d / rhs.d;
      default: throw std::runtime_error("unsupported type for '/' operator");
    }
  }

  inline auto operator%(const any& rhs_) const -> any {
    auto& lhs = extractValue(*this);
    auto& rhs = extractValue(rhs_);
    assert(lhs.type == rhs.type);
    switch (lhs.type) {
      case Type::Integer: return lhs.i % rhs.i;
      default: throw std::runtime_error("unsupported type for '%' operator");
    }
  }

  #undef returnValue
};

template <typename T>
constexpr auto cast(const any& a) {
  return *(a.cast<T>());
}

} // namespace PureScript

#endif // PS_memory_HH
