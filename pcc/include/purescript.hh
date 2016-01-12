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

#if defined(DEBUG)
  #include <cassert>
#else
  #define NDEBUG
  #undef assert
  #define assert(x)
#endif

#include <memory>
#include <functional>
#include <string>
#include <vector>
#include <unordered_map>
#include <stdexcept>
#include "map_key.hh"

namespace PureScript {

using string = std::string;
using runtime_error = std::runtime_error;
using nullptr_t = std::nullptr_t;

const bool undefined = false;

// A variant data class designed to provide some features of dynamic typing.
//
class any {

  public:
  enum class Type {
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
  static constexpr as_thunk unthunk = as_thunk{};

  using map    = std::unordered_map<const map_key_t, const any, map_key_t::hasher, map_key_t::equal>;
  using vector = std::vector<any>;
  using fn     = std::function<any(const any&)>;
  using eff_fn = std::function<any()>;
  using thunk  = std::function<const any& (const as_thunk)>;

  template <typename T>
  using shared = std::shared_ptr<T>;

  template <typename T, typename... Args>
  inline static auto make_shared(Args&&... args) -> decltype(std::make_shared<T>(std::forward<Args>(args)...)) {
    return std::make_shared<T>(std::forward<Args>(args)...);
  }

  private:
  union {
    mutable long            i;
    mutable double          d;
    mutable char            c;
    mutable bool            b;
    mutable shared<string>  s;
    mutable shared<map>     m;
    mutable shared<vector>  v;
    mutable shared<fn>      f;
    mutable shared<eff_fn>  e;
    mutable shared<thunk>   t;
    mutable shared<void>    p;
  };

  public:

  any(const long val) : type(Type::Integer), i(val) {}
  any(const int val) : type(Type::Integer), i(val) {}
  any(const unsigned int val) : type(Type::Integer), i(val) {}
  any(const double val) : type(Type::Double), d(val) {}
  any(const char val) : type(Type::Character), c(val) {}

  template <typename T, typename = typename std::enable_if<std::is_same<bool,T>::value>::type>
  any(const T val) : type(Type::Boolean), b(val) {}

  any(const string& val) : type(Type::String), s(make_shared<string>(val)) {}
  any(string&& val) noexcept : type(Type::String), s(make_shared<string>(std::move(val))) {}
  any(const char val[]) : type(Type::String), s(make_shared<string>(val)) {}

  any(const map& val) : type(Type::Map), m(make_shared<map>(val)) {}
  any(map&& val) noexcept : type(Type::Map), m(make_shared<map>(std::move(val))) {}

  any(const vector& val) : type(Type::Vector), v(make_shared<vector>(val)) {}
  any(vector&& val) noexcept : type(Type::Vector), v(make_shared<vector>(std::move(val))) {}

  template <typename T, typename = typename std::enable_if<!std::is_same<any,T>::value>::type>
  any(const T& val, typename std::enable_if<std::is_assignable<fn,T>::value>::type* = 0)
    : type(Type::Function), f(make_shared<fn>(val)) {}

  template <typename T>
  any(const T& val, typename std::enable_if<std::is_assignable<eff_fn,T>::value>::type* = 0)
    : type(Type::EffFunction), e(make_shared<eff_fn>(val)) {}

  template <typename T>
  any(const T& val, typename std::enable_if<std::is_assignable<thunk,T>::value>::type* = 0)
    : type(Type::Thunk), t(make_shared<thunk>(val)) {}

  template <typename T>
  any(const T& val, typename std::enable_if<std::is_assignable<shared<void>,T>::value>::type* = 0)
    : type(Type::Pointer), p(val) {}

  template <typename T>
  any(T&& val, typename std::enable_if<std::is_assignable<shared<void>,T>::value>::type* = 0) noexcept
    : type(Type::Pointer), p(std::move(val)) {}

  any(std::nullptr_t) : type(Type::Pointer), p(nullptr) {}

  any(const any&);
  any(any&&) noexcept;

  auto operator=(const any&) -> any&;
  auto operator=(any&) noexcept -> any&;
  auto operator=(any&&) noexcept -> any&;

  any() = delete;
  ~any();

  template <typename T>
  auto cast() const -> typename std::enable_if<std::is_same<T, long>::value, T>::type;

  template <typename T>
  auto cast() const -> typename std::enable_if<std::is_same<T, double>::value, T>::type;

  template <typename T>
  auto cast() const -> typename std::enable_if<std::is_same<T, char>::value, T>::type;

  template <typename T>
  auto cast() const -> typename std::enable_if<std::is_same<T, bool>::value, T>::type;

  template <typename T>
  auto cast() const -> typename std::enable_if<std::is_same<T, string>::value, const T&>::type;

  template <typename T>
  auto cast() const -> typename std::enable_if<std::is_same<T, const char*>::value, const T>::type;

  template <typename T>
  auto cast() const -> typename std::enable_if<std::is_same<T, map>::value, const T&>::type;

  template <typename T>
  auto cast() const -> typename std::enable_if<std::is_same<T, vector>::value, const T&>::type;

  template <typename T>
  auto cast() const -> typename std::enable_if<std::is_assignable<shared<void>,T>::value, typename T::element_type*>::type {
    return static_cast<typename T::element_type*>(extractPointer());
  }

  auto operator()(const any&) const -> any;

  auto operator()(const as_thunk) const -> const any&;

  auto operator()() const -> any;

  operator long() const;
  operator double() const;
  operator bool() const;

  operator const string&() const;
  operator const map&() const;
  operator const vector&() const;

  auto operator[](const map_key_t&) const -> const any&;
  auto operator[](const vector::size_type) const -> const any&;
  auto operator[](const any&) const -> const any&;

  auto extractPointer() const -> void*;

  static auto extract_value(const any&) -> const any&;

  auto operator==(const any&) const -> bool;
  auto operator==(const long) const -> bool;
  auto operator==(const double) const -> bool;
  auto operator==(const char) const -> bool;
  auto operator==(const bool) const -> bool;
  auto operator==(const string&) const -> bool;
  auto operator==(const char * const) const -> bool;

  auto operator!=(const any&) const -> bool;
  auto operator!=(const long) const -> bool;
  auto operator!=(const double) const -> bool;
  auto operator!=(const char) const -> bool;
  auto operator!=(const bool) const -> bool;
  auto operator!=(const string&) const -> bool;
  auto operator!=(const char * const) const -> bool;

  auto operator<(const any&) const -> bool;
  auto operator<(const long) const -> bool;
  auto operator<(const double) const -> bool;
  auto operator<(const char) const -> bool;
  auto operator<(const bool) const -> bool;
  auto operator<(const string&) const -> bool;
  auto operator<(const char * const) const -> bool;

  auto operator<=(const any&) const -> bool;
  auto operator<=(const long) const -> bool;
  auto operator<=(const double) const -> bool;
  auto operator<=(const char) const -> bool;
  auto operator<=(const bool) const -> bool;
  auto operator<=(const string&) const -> bool;
  auto operator<=(const char * const) const -> bool;

  auto operator>(const any&) const -> bool;
  auto operator>(const long) const -> bool;
  auto operator>(const double) const -> bool;
  auto operator>(const char) const -> bool;
  auto operator>(const bool) const -> bool;
  auto operator>(const string&) const -> bool;
  auto operator>(const char * const) const -> bool;

  auto operator>=(const any&) const -> bool;
  auto operator>=(const long) const -> bool;
  auto operator>=(const double) const -> bool;
  auto operator>=(const char) const -> bool;
  auto operator>=(const bool) const -> bool;
  auto operator>=(const string&) const -> bool;
  auto operator>=(const char * const) const -> bool;

  auto operator+(const any&) const -> any;
  auto operator+(const long) const -> long;
  auto operator+(const double) const -> double;
  auto operator+(const char) const -> char;
  auto operator+(const string&) const -> string;
  auto operator+(const char * const) const -> string;

  auto operator-(const any&) const -> any;
  auto operator-(const long) const -> long;
  auto operator-(const double) const -> double;
  auto operator-(const char) const -> char;

  auto operator*(const any&) const -> any;
  auto operator*(const long) const -> long;
  auto operator*(const double) const -> double;

  auto operator/(const any&) const -> any;
  auto operator/(const long) const -> long;
  auto operator/(const double) const -> double;

  auto operator%(const any&) const -> any;
  auto operator%(const long) const -> long;

  auto operator-() const -> any; // unary negate

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

  friend auto operator==(const string&, const any&) -> bool;
  friend auto operator!=(const string&, const any&) -> bool;
  friend auto operator< (const string&, const any&) -> bool;
  friend auto operator<=(const string&, const any&) -> bool;
  friend auto operator> (const string&, const any&) -> bool;
  friend auto operator>=(const string&, const any&) -> bool;

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

  friend auto operator+(const string&, const any&) -> string;
  friend auto operator+(const char* const, const any&) -> string;

};

} // namespace PureScript

#endif // PureScript_HH
