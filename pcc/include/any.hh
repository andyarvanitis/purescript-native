///////////////////////////////////////////////////////////////////////////////
//
// Module      :  any.hh
// Copyright   :  (c) Andy Arvanitis 2015
// License     :  MIT
//
// Maintainer  :  Andy Arvanitis <andy.arvanitis@gmail.com>
// Stability   :  experimental
// Portability :
//
// A variant data class designed to provide some features of dynamic typing.
//
///////////////////////////////////////////////////////////////////////////////
//
#ifndef PS_ANY_HH
#define PS_ANY_HH

#if defined(DEBUG)
  #include <cassert>
#else
  #define NDEBUG
  #undef assert
  #define assert(x)
#endif

#include <memory>
#include <functional>
#include <unordered_map>
#include <string>
#include <vector>
#include <stdexcept>
#include <iostream>

namespace PureScript {

class any {

  public:
  enum class Type : std::int8_t {
    Unknown,
    Integer,
    Double,
    Character,
    Boolean,
    String,
    Map,
    Vector,
    Function,
    EffFunction,
    Thunk,
    Pointer
  };

  mutable Type type = Type::Unknown;

  struct as_thunk {
  };
  constexpr static const as_thunk unthunk = as_thunk{};

  struct map_key_t {
    const uint32_t hash;
    constexpr map_key_t(const uint32_t hash) : hash(hash) {}
  };

  // For compile-time hashing of map key names
  //
  static constexpr auto djb2(const char s[], const uint32_t hash = 5381) -> uint32_t {
    return !s[0] ? hash : djb2(s + 1, 33 * hash ^ s[0]);
  }

  class map_key_hash {
  public:
    constexpr auto operator() (const map_key_t key) const -> size_t {
      return key.hash;
    }
  };

  class map_key_equal {
  public:
    constexpr auto operator() (const map_key_t key1, const map_key_t key2) const -> bool {
      return key1.hash == key2.hash;
    }
  };

  using string = std::string;
  using map    = std::unordered_map<map_key_t, const any, map_key_hash, map_key_equal>;
  using vector = std::vector<any>;
  using fn     = std::function<any(const any&)>;
  using eff_fn = std::function<any()>;
  using thunk  = std::function<const any& (const as_thunk)>;

  using shared_map    = std::shared_ptr<map>;
  using shared_vector = std::shared_ptr<vector>;

  using unique_map    = std::shared_ptr<map>;
  using unique_vector = std::shared_ptr<vector>;

  template <typename T>
  using shared = std::shared_ptr<T>;

  using shared_void_ptr = std::shared_ptr<void>;

  template <typename T>
  static constexpr auto make_shared(const T& arg) {
    return std::make_shared<T>(arg);
  }

  template <typename T>
  static constexpr auto make_shared(T&& arg) {
    return std::make_shared<T>(std::move(arg));
  }

  template <typename T, typename U>
  static constexpr auto static_cast_shared(U arg) {
    return static_cast<T*>(arg.get());
  }

  private:
  union {
    mutable long            i;
    mutable double          d;
    mutable char            c;
    mutable bool            b;
    mutable string          s;
    mutable shared_map      m;
    mutable shared_vector   v;
    mutable fn              f;
    mutable eff_fn          e;
    mutable thunk           t;
    mutable shared_void_ptr p;
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
  any(const char val[]) : type(Type::String), s(val) {}

  any(const map& val) : type(Type::Map), m(make_shared<map>(val)) {}
  any(map&& val) : type(Type::Map), m(make_shared<map>(std::move(val))) {}

  any(const shared_map& val) : type(Type::Map), m(val) {}
  any(shared_map&& val) : type(Type::Map), m(std::move(val)) {}

  any(const vector& val) : type(Type::Vector), v(make_shared<vector>(val)) {}
  any(vector&& val) : type(Type::Vector), v(make_shared<vector>(std::move(val))) {}

  any(const shared_vector& val) : type(Type::Vector), v(val) {}
  any(shared_vector&& val) : type(Type::Vector), v(std::move(val)) {}

  template <typename T>
  constexpr any(const T& val, typename std::enable_if<std::is_assignable<fn,T>::value>::type* = 0)
    : type(Type::Function), f(val) {}

  // template <typename T>
  // any(T&& val, typename std::enable_if<std::is_assignable<fn,T>::value>::type* = 0)
  //   : type(Type::Function), f(std::move(val)) {}

  template <typename T>
  constexpr any(const T& val, typename std::enable_if<std::is_assignable<eff_fn,T>::value>::type* = 0)
    : type(Type::EffFunction), e(val) {}

  template <typename T>
  constexpr any(const T& val, typename std::enable_if<std::is_assignable<thunk,T>::value>::type* = 0)
    : type(Type::Thunk), t(val) {}

  template <typename T>
  any(const T& val, typename std::enable_if<std::is_assignable<shared_void_ptr,T>::value>::type* = 0)
    : type(Type::Pointer), p(val) {
    }

  constexpr
  any(std::nullptr_t) : type(Type::Pointer), p(nullptr) {}

  template <typename T>
  any(T&& val, typename std::enable_if<std::is_assignable<shared_void_ptr,T>::value>::type* = 0)
    : type(Type::Pointer), p(std::move(val)) {
    }

  any(const any& val) : type(val.type) {
    switch (type) {
      case Type::Integer:         i = val.i;                         break;
      case Type::Double:          d = val.d;                         break;
      case Type::Character:       c = val.c;                         break;
      case Type::Boolean:         b = val.b;                         break;
      case Type::String:          new (&s) string          (val.s);  break;
      case Type::Map:             new (&m) shared_map      (val.m);  break;
      case Type::Vector:          new (&v) shared_vector   (val.v);  break;
      case Type::Function:        new (&f) fn              (val.f);  break;
      case Type::EffFunction:     new (&e) eff_fn          (val.e);  break;
      case Type::Thunk:           new (&t) thunk           (val.t);  break;
      case Type::Pointer:         new (&p) shared_void_ptr (val.p);  break;

      default: throw std::runtime_error("unsupported type in copy ctor");
    }
  }

  auto swap(any&& val) const {
    type = val.type;
    switch (type) {
      case Type::Integer:         i = val.i;                                    break;
      case Type::Double:          d = val.d;                                    break;
      case Type::Character:       c = val.c;                                    break;
      case Type::Boolean:         b = val.b;                                    break;
      case Type::String:          new (&s) string          (std::move(val.s));  break;
      case Type::Map:             new (&m) shared_map      (std::move(val.m));  break;
      case Type::Vector:          new (&v) shared_vector   (std::move(val.v));  break;
      case Type::Function:        new (&f) fn              (std::move(val.f));  break;
      case Type::EffFunction:     new (&e) eff_fn          (std::move(val.e));  break;
      case Type::Thunk:           new (&t) thunk           (std::move(val.t));  break;
      case Type::Pointer:         new (&p) shared_void_ptr (std::move(val.p));  break;

      default: throw std::runtime_error("unsupported type in move ctor");
    }
  }

  any(any&& val) {
    swap(std::move(val));
  }

  auto operator=(any val) -> any& {
    swap(std::move(val));
    return *this;
  }

  any() = delete;
  ~any() {
    // std::cout << "destroy" << std::endl;
    switch (type) {
      case Type::Integer:         ;                     break;
      case Type::Double:          ;                     break;
      case Type::Character:       ;                     break;
      case Type::Boolean:         ;                     break;
      case Type::String:          s.~string();          break;
      case Type::Map:             m.~shared_map();      break;
      case Type::Vector:          v.~shared_vector();   break;
      case Type::Function:        f.~fn();              break;
      case Type::EffFunction:     e.~eff_fn();          break;
      case Type::Thunk:           t.~thunk();           break;
      case Type::Pointer:         p.~shared_void_ptr(); break;

      default: throw std::runtime_error("unsupported type in destructor");
    }
  };

  #define RETURN_VALUE(TYPE, VAL, FN) \
    if (type == TYPE) { \
      return FN(VAL); \
    } else { \
      assert(type == Type::Thunk); \
      const any& value = t(unthunk); \
      assert(value.type == TYPE); \
      return FN(value.VAL); \
    }

  template <typename T>
  constexpr auto cast() const -> typename std::enable_if<std::is_same<T, long>::value, T>::type {
    RETURN_VALUE(Type::Integer, i,)
  }

  template <typename T>
  constexpr auto cast() const -> typename std::enable_if<std::is_same<T, double>::value, T>::type {
    RETURN_VALUE(Type::Double, d,)
  }

  template <typename T>
  constexpr auto cast() const -> typename std::enable_if<std::is_same<T, char>::value, T>::type {
    RETURN_VALUE(Type::Character, c,)
  }

  template <typename T>
  constexpr auto cast() const -> typename std::enable_if<std::is_same<T, bool>::value, T>::type {
    RETURN_VALUE(Type::Boolean, b,)
  }

  template <typename T>
  constexpr auto cast() const -> typename std::enable_if<std::is_same<T, string>::value, const T&>::type {
    RETURN_VALUE(Type::String, s,)
  }

  template <typename T>
  constexpr auto cast() const -> typename std::enable_if<std::is_same<T, map>::value, const T&>::type {
    RETURN_VALUE(Type::Map, m, *)
  }

  template <typename T>
  constexpr auto cast() const -> typename std::enable_if<std::is_same<T, vector>::value, const T&>::type {
    RETURN_VALUE(Type::Vector, v, *)
  }

  template <typename T>
  constexpr auto cast() const ->
      typename std::enable_if<std::is_assignable<shared_void_ptr,T>::value, typename T::element_type*>::type {
    RETURN_VALUE(Type::Pointer, p, static_cast_shared<typename T::element_type>)
  }

  auto operator()(const any arg) const -> any {
    RETURN_VALUE(Type::Function, f(arg),)
  }

  auto operator()(const as_thunk) const -> const any& {
    assert(type == Type::Thunk);
    return t(unthunk);
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
      const any& value = t(unthunk);
      return call(value);
    }
  }

  explicit operator long() const {
    return i;
  }

  explicit operator double() const {
    return d;
  }

  explicit operator bool() const {
    return b;
  }

  operator const string&() const {
    RETURN_VALUE(Type::String, s,)
  }

  operator const map&() const {
    RETURN_VALUE(Type::Map, m, *)
  }

  operator const vector&() const {
    RETURN_VALUE(Type::Vector, v, *)
  }

  auto extractPointer() const -> const void* {
    RETURN_VALUE(Type::Pointer, p.get(),)
  }

  inline auto operator[](const map_key_t rhs) const -> const any& {
    RETURN_VALUE(Type::Map, m->at(rhs),)
  }

  inline auto operator[](const vector::size_type rhs) const -> const any& {
    RETURN_VALUE(Type::Vector, v->at(rhs),)
  }

  inline auto operator[](const any& rhs) const -> const any& {
    RETURN_VALUE(Type::Vector, v->at(rhs.cast<long>()),)
  }

  #undef RETURN_VALUE

  static constexpr auto extract_value(const any& a) -> const any& {
    return a.type != Type::Thunk ? a : a.t(unthunk);
  }

  #define DEFINE_OPERATOR_RHS(K, T, OP, V, R) \
  inline auto operator OP(const T rhs) const -> R { \
    auto& lhs = extract_value(*this); \
    assert(lhs.type == Type::K); \
    return lhs.V OP rhs; \
  }

  inline auto operator==(const any& rhs_) const -> bool {
    auto& lhs = extract_value(*this);
    auto& rhs = extract_value(rhs_);
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

  DEFINE_OPERATOR_RHS(Integer,   long,   ==, i, bool)
  DEFINE_OPERATOR_RHS(Double,    double, ==, d, bool)
  DEFINE_OPERATOR_RHS(Character, char,   ==, c, bool)
  DEFINE_OPERATOR_RHS(Boolean,   bool,   ==, b, bool)
  DEFINE_OPERATOR_RHS(String, char* const, ==, s, bool)

  inline auto operator!=(const any& rhs_) const -> bool {
    auto& lhs = extract_value(*this);
    auto& rhs = extract_value(rhs_);
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

  DEFINE_OPERATOR_RHS(Integer,   long,   !=, i, bool)
  DEFINE_OPERATOR_RHS(Double,    double, !=, d, bool)
  DEFINE_OPERATOR_RHS(Character, char,   !=, c, bool)
  DEFINE_OPERATOR_RHS(Boolean,   bool,   !=, b, bool)
  DEFINE_OPERATOR_RHS(String, char* const, !=, s, bool)

  inline auto operator<(const any& rhs_) const -> bool {
    auto& lhs = extract_value(*this);
    auto& rhs = extract_value(rhs_);
    assert(lhs.type == rhs.type);
    switch (lhs.type) {
      case Type::Integer:   return lhs.i < rhs.i;
      case Type::Double:    return lhs.d < rhs.d;
      case Type::Character: return lhs.c < rhs.c;
      case Type::Boolean:   return lhs.b < rhs.b;
      case Type::String:    return lhs.s < rhs.s;
      default: throw std::runtime_error("unsupported type for '<' operator");
    }
  }

  DEFINE_OPERATOR_RHS(Integer,   long,   <, i, bool)
  DEFINE_OPERATOR_RHS(Double,    double, <, d, bool)
  DEFINE_OPERATOR_RHS(Character, char,   <, c, bool)
  DEFINE_OPERATOR_RHS(Boolean,   bool,   <, b, bool)
  DEFINE_OPERATOR_RHS(String, char* const, <, s, bool)

  inline auto operator<=(const any& rhs_) const -> bool {
    auto& lhs = extract_value(*this);
    auto& rhs = extract_value(rhs_);
    assert(lhs.type == rhs.type);
    switch (lhs.type) {
      case Type::Integer:   return lhs.i <= rhs.i;
      case Type::Double:    return lhs.d <= rhs.d;
      case Type::Character: return lhs.c <= rhs.c;
      case Type::Boolean:   return lhs.b <= rhs.b;
      case Type::String:    return lhs.s <= rhs.s;
      default: throw std::runtime_error("unsupported type for '<' operator");
    }
  }

  DEFINE_OPERATOR_RHS(Integer,   long,   <=, i, bool)
  DEFINE_OPERATOR_RHS(Double,    double, <=, d, bool)
  DEFINE_OPERATOR_RHS(Character, char,   <=, c, bool)
  DEFINE_OPERATOR_RHS(Boolean,   bool,   <=, b, bool)
  DEFINE_OPERATOR_RHS(String, char* const, <=, s, bool)

  inline auto operator>(const any& rhs_) const -> bool {
    auto& lhs = extract_value(*this);
    auto& rhs = extract_value(rhs_);
    assert(lhs.type == rhs.type);
    switch (lhs.type) {
      case Type::Integer:   return lhs.i > rhs.i;
      case Type::Double:    return lhs.d > rhs.d;
      case Type::Character: return lhs.c > rhs.c;
      case Type::Boolean:   return lhs.b > rhs.b;
      case Type::String:    return lhs.s > rhs.s;
      default: throw std::runtime_error("unsupported type for '>' operator");
    }
  }

  DEFINE_OPERATOR_RHS(Integer,   long,   >, i, bool)
  DEFINE_OPERATOR_RHS(Double,    double, >, d, bool)
  DEFINE_OPERATOR_RHS(Character, char,   >, c, bool)
  DEFINE_OPERATOR_RHS(Boolean,   bool,   >, b, bool)
  DEFINE_OPERATOR_RHS(String, char* const, >, s, bool)

  inline auto operator>=(const any& rhs_) const -> bool {
    auto& lhs = extract_value(*this);
    auto& rhs = extract_value(rhs_);
    assert(lhs.type == rhs.type);
    switch (lhs.type) {
      case Type::Integer:   return lhs.i >= rhs.i;
      case Type::Double:    return lhs.d >= rhs.d;
      case Type::Character: return lhs.c >= rhs.c;
      case Type::Boolean:   return lhs.b >= rhs.b;
      case Type::String:    return lhs.s >= rhs.s;
      default: throw std::runtime_error("unsupported type for '>' operator");
    }
  }

  DEFINE_OPERATOR_RHS(Integer,   long,   >=, i, bool)
  DEFINE_OPERATOR_RHS(Double,    double, >=, d, bool)
  DEFINE_OPERATOR_RHS(Character, char,   >=, c, bool)
  DEFINE_OPERATOR_RHS(Boolean,   bool,   >=, b, bool)
  DEFINE_OPERATOR_RHS(String, char* const, >=, s, bool)

  inline auto operator+(const any& rhs_) const -> any {
    auto& lhs = extract_value(*this);
    auto& rhs = extract_value(rhs_);
    assert(lhs.type == rhs.type);
    switch (lhs.type) {
      case Type::Integer:   return lhs.i + rhs.i;
      case Type::Double:    return lhs.d + rhs.d;
      case Type::Character: return lhs.c + rhs.c;
      case Type::String:    return lhs.s + rhs.s;
      default: throw std::runtime_error("unsupported for '+' operator");
    }
  }

  DEFINE_OPERATOR_RHS(Integer,   long,   +, i, long)
  DEFINE_OPERATOR_RHS(Double,    double, +, d, double)
  DEFINE_OPERATOR_RHS(Character, char,   +, c, char)
  DEFINE_OPERATOR_RHS(String, char* const, +, s, string)

  inline auto operator-(const any& rhs_) const -> any {
    auto& lhs = extract_value(*this);
    auto& rhs = extract_value(rhs_);
    assert(lhs.type == rhs.type);
    switch (lhs.type) {
      case Type::Integer:   return lhs.i - rhs.i;
      case Type::Double:    return lhs.d - rhs.d;
      case Type::Character: return lhs.c - rhs.c;
      default: throw std::runtime_error("unsupported type for '-' binary operator");
    }
  }

  DEFINE_OPERATOR_RHS(Integer,   long,   -, i, long)
  DEFINE_OPERATOR_RHS(Double,    double, -, d, double)
  DEFINE_OPERATOR_RHS(Character, char,   -, c, char)

  inline auto operator*(const any& rhs_) const -> any {
    auto& lhs = extract_value(*this);
    auto& rhs = extract_value(rhs_);
    assert(lhs.type == rhs.type);
    switch (lhs.type) {
      case Type::Integer: return lhs.i * rhs.i;
      case Type::Double:  return lhs.d * rhs.d;
      default: throw std::runtime_error("unsupported type for '*' operator");
    }
  }

  DEFINE_OPERATOR_RHS(Integer,   long,   *, i, long)
  DEFINE_OPERATOR_RHS(Double,    double, *, d, double)

  inline auto operator/(const any& rhs_) const -> any {
    auto& lhs = extract_value(*this);
    auto& rhs = extract_value(rhs_);
    assert(lhs.type == rhs.type);
    switch (lhs.type) {
      case Type::Integer: return lhs.i / rhs.i;
      case Type::Double:  return lhs.d / rhs.d;
      default: throw std::runtime_error("unsupported type for '/' operator");
    }
  }

  DEFINE_OPERATOR_RHS(Integer,   long,   /, i, long)
  DEFINE_OPERATOR_RHS(Double,    double, /, d, double)

  inline auto operator%(const any& rhs_) const -> any {
    auto& lhs = extract_value(*this);
    auto& rhs = extract_value(rhs_);
    assert(lhs.type == rhs.type);
    switch (lhs.type) {
      case Type::Integer: return lhs.i % rhs.i;
      default: throw std::runtime_error("unsupported type for '%' operator");
    }
  }

  DEFINE_OPERATOR_RHS(Integer,   long, %, i, long)

  inline auto operator-() const -> any {
    auto& lhs = extract_value(*this);
    switch (lhs.type) {
      case Type::Integer: return (- lhs.i);
      case Type::Double:  return (- lhs.d);
      default: throw std::runtime_error("unsupported type for unary '-' operator");
    }
  }

  friend auto operator==(const long, const any&) -> bool;
  friend auto operator!=(const long, const any&) -> bool;
  friend auto operator< (const long, const any&) -> bool;
  friend auto operator<=(const long, const any&) -> bool;
  friend auto operator> (const long, const any&) -> bool;
  friend auto operator>=(const long, const any&) -> bool;

  friend auto operator==(const double, const any&) -> bool;
  friend auto operator!=(const double, const any&) -> bool;
  friend auto operator< (const double, const any&) -> bool;
  friend auto operator<=(const double, const any&) -> bool;
  friend auto operator> (const double, const any&) -> bool;
  friend auto operator>=(const double, const any&) -> bool;

  friend auto operator==(const char, const any&) -> bool;
  friend auto operator!=(const char, const any&) -> bool;
  friend auto operator< (const char, const any&) -> bool;
  friend auto operator<=(const char, const any&) -> bool;
  friend auto operator> (const char, const any&) -> bool;
  friend auto operator>=(const char, const any&) -> bool;

  friend auto operator==(const bool, const any&) -> bool;
  friend auto operator!=(const bool, const any&) -> bool;
  friend auto operator< (const bool, const any&) -> bool;
  friend auto operator<=(const bool, const any&) -> bool;
  friend auto operator> (const bool, const any&) -> bool;
  friend auto operator>=(const bool, const any&) -> bool;

  friend auto operator==(const char* const, const any&) -> bool;
  friend auto operator!=(const char* const, const any&) -> bool;
  friend auto operator< (const char* const, const any&) -> bool;
  friend auto operator<=(const char* const, const any&) -> bool;
  friend auto operator> (const char* const, const any&) -> bool;
  friend auto operator>=(const char* const, const any&) -> bool;

  friend auto operator+(const long, const any&) -> long;
  friend auto operator-(const long, const any&) -> long;
  friend auto operator*(const long, const any&) -> long;
  friend auto operator/(const long, const any&) -> long;
  friend auto operator%(const long, const any&) -> long;

  friend auto operator+(const double, const any&) -> double;
  friend auto operator-(const double, const any&) -> double;
  friend auto operator*(const double, const any&) -> double;
  friend auto operator/(const double, const any&) -> double;

  friend auto operator+(const char, const any&) -> char;
  friend auto operator-(const char, const any&) -> char;

  friend auto operator+(const char* const, const any&) -> string;

};

#define DEFINE_OPERATOR_LHS(K, T, OP, V, R) \
inline auto operator OP(const T lhs, const any& rhs_) -> R { \
  auto& rhs = any::extract_value(rhs_); \
  assert(rhs.type == any::Type::K); \
  return lhs OP rhs.V; \
}

DEFINE_OPERATOR_LHS(Integer, long, ==, i, bool)
DEFINE_OPERATOR_LHS(Integer, long, !=, i, bool)
DEFINE_OPERATOR_LHS(Integer, long, <,  i, bool)
DEFINE_OPERATOR_LHS(Integer, long, <=, i, bool)
DEFINE_OPERATOR_LHS(Integer, long, >,  i, bool)
DEFINE_OPERATOR_LHS(Integer, long, >=, i, bool)

DEFINE_OPERATOR_LHS(Double, double, ==, d, bool)
DEFINE_OPERATOR_LHS(Double, double, !=, d, bool)
DEFINE_OPERATOR_LHS(Double, double, <,  d, bool)
DEFINE_OPERATOR_LHS(Double, double, <=, d, bool)
DEFINE_OPERATOR_LHS(Double, double, >,  d, bool)
DEFINE_OPERATOR_LHS(Double, double, >=, d, bool)

DEFINE_OPERATOR_LHS(Character, char, ==, c, bool)
DEFINE_OPERATOR_LHS(Character, char, !=, c, bool)
DEFINE_OPERATOR_LHS(Character, char, <,  c, bool)
DEFINE_OPERATOR_LHS(Character, char, <=, c, bool)
DEFINE_OPERATOR_LHS(Character, char, >,  c, bool)
DEFINE_OPERATOR_LHS(Character, char, >=, c, bool)

DEFINE_OPERATOR_LHS(Boolean, bool, ==, b, bool)
DEFINE_OPERATOR_LHS(Boolean, bool, !=, b, bool)
DEFINE_OPERATOR_LHS(Boolean, bool, <,  b, bool)
DEFINE_OPERATOR_LHS(Boolean, bool, <=, b, bool)
DEFINE_OPERATOR_LHS(Boolean, bool, >,  b, bool)
DEFINE_OPERATOR_LHS(Boolean, bool, >=, b, bool)

DEFINE_OPERATOR_LHS(String, char* const, ==, s, bool)
DEFINE_OPERATOR_LHS(String, char* const, !=, s, bool)
DEFINE_OPERATOR_LHS(String, char* const, <,  s, bool)
DEFINE_OPERATOR_LHS(String, char* const, <=, s, bool)
DEFINE_OPERATOR_LHS(String, char* const, >,  s, bool)
DEFINE_OPERATOR_LHS(String, char* const, >=, s, bool)

DEFINE_OPERATOR_LHS(Integer, long, +, i, long)
DEFINE_OPERATOR_LHS(Integer, long, -, i, long)
DEFINE_OPERATOR_LHS(Integer, long, *, i, long)
DEFINE_OPERATOR_LHS(Integer, long, /, i, long)
DEFINE_OPERATOR_LHS(Integer, long, %, i, long)

DEFINE_OPERATOR_LHS(Double, double, +, d, double)
DEFINE_OPERATOR_LHS(Double, double, -, d, double)
DEFINE_OPERATOR_LHS(Double, double, *, d, double)
DEFINE_OPERATOR_LHS(Double, double, /, d, double)

DEFINE_OPERATOR_LHS(Character, char, +, i, char)
DEFINE_OPERATOR_LHS(Character, char, -, i, char)

DEFINE_OPERATOR_LHS(String, char* const, +, s, any::string)

#undef DEFINE_OPERATOR_RHS
#undef DEFINE_OPERATOR_LHS

// Compile-time string key literals
//
constexpr auto operator "" _key(const char s[], size_t) -> const any::map_key_t {
  return any::map_key_t(any::djb2(s));
}

} // namespace PureScript

#endif // PS_ANY_HH
