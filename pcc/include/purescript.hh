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
#include <utility>
#include <stdexcept>

#define DECLARE_OPERATOR(op, ty, rty) \
  friend auto operator op (const any&, ty) -> rty; \
  friend auto operator op (ty, const any&) -> rty;

#define DECLARE_COMPARISON_OPERATOR(op) \
  friend auto operator op (const any&, const any&) -> bool; \
  DECLARE_OPERATOR(op, long, bool) \
  DECLARE_OPERATOR(op, double, bool) \
  DECLARE_OPERATOR(op, char, bool) \
  DECLARE_OPERATOR(op, const char *, bool)


namespace PureScript {

using string = const char *;
using nullptr_t = std::nullptr_t;

// Workaround for missing C++11 version in gcc
class runtime_error : public std::runtime_error {
public:
  runtime_error(const char message[]) : std::runtime_error(std::string(message)) {}
};

const bool undefined = false;

// A variant data class designed to provide some features of dynamic typing.
//
class any {

  public:
  enum class Type : int8_t {
    Unknown,
    Integer,
    Double,
    Character,
    Boolean,
    StringLiteral,
    String,
    Map,
    Vector,
    Function,
    Closure,
    EffFunction,
    Thunk,
    Pointer
  };

  mutable Type type = Type::Unknown;

  struct as_thunk {
  };
  static constexpr as_thunk unthunk = as_thunk{};

  using map     = std::vector<std::pair<const char * const, const any>>;
  using vector  = std::vector<any>;
  using fn      = auto (*)(const any&) -> any;
  using closure = std::function<any(const any&)>;
  using eff_fn  = std::function<any()>;
  using thunk   = auto (*)(const as_thunk) -> const any&;

  template <typename T>
  using shared = std::shared_ptr<T>;

  template <typename T, typename... Args>
  inline static auto make_shared(Args&&... args) -> decltype(std::make_shared<T>(std::forward<Args>(args)...)) {
    return std::make_shared<T>(std::forward<Args>(args)...);
  }

  private:
  union {
    mutable long                 i;
    mutable double               d;
    mutable char                 c;
    mutable bool                 b;
    mutable string               r;
    mutable shared<std::string>  s;
    mutable shared<map>          m;
    mutable shared<vector>       v;
    mutable fn                   f;
    mutable shared<closure>      l;
    mutable shared<eff_fn>       e;
    mutable thunk                t;
    mutable shared<void>         p;
  };

  public:

  any(const long val) : type(Type::Integer), i(val) {}
  any(const int val) : type(Type::Integer), i(val) {}
  any(const unsigned int val) : type(Type::Integer), i(val) {}
  any(const double val) : type(Type::Double), d(val) {}
  any(const char val) : type(Type::Character), c(val) {}

  template <typename T, typename = typename std::enable_if<std::is_same<bool,T>::value>::type>
  any(const T val) : type(Type::Boolean), b(val) {}

  template <size_t N>
  any(const char (&val)[N]) : type(Type::StringLiteral), r(val) {}
  any(char * val) : type(Type::String), s(make_shared<std::string>(val)) {}

  any(const std::string& val) : type(Type::String), s(make_shared<std::string>(val)) {}
  any(std::string&& val) : type(Type::String), s(make_shared<std::string>(std::move(val))) {}

  any(const shared<std::string>& val) : type(Type::String), s(val) {}
  any(shared<std::string>&& val) noexcept : type(Type::String), s(std::move(val)) {}

  any(const map& val) : type(Type::Map), m(make_shared<map>(val)) {}
  any(map&& val) noexcept : type(Type::Map), m(make_shared<map>(std::move(val))) {}

  any(const vector& val) : type(Type::Vector), v(make_shared<vector>(val)) {}
  any(vector&& val) noexcept : type(Type::Vector), v(make_shared<vector>(std::move(val))) {}

  template <typename T>
  any(const T& val, typename std::enable_if<std::is_convertible<T,fn>::value>::type* = 0)
    : type(Type::Function), f(val) {}

  template <typename T, typename = typename std::enable_if<!std::is_same<any,T>::value &&
                                                           !std::is_convertible<T,fn>::value>::type>
  any(const T& val, typename std::enable_if<std::is_assignable<closure,T>::value>::type* = 0)
    : type(Type::Closure), l(make_shared<closure>(val)) {}

  template <typename T>
  any(const T& val, typename std::enable_if<std::is_assignable<eff_fn,T>::value>::type* = 0)
    : type(Type::EffFunction), e(make_shared<eff_fn>(val)) {}

  template <typename T>
  any(const T& val, typename std::enable_if<std::is_convertible<T,thunk>::value>::type* = 0)
    : type(Type::Thunk), t(val) {}

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
  auto cast() const -> typename std::enable_if<std::is_same<T, string>::value, T>::type;

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

  operator string() const;
  operator const map&() const;
  operator const vector&() const;

  auto operator[](const char[]) const -> const any&;
  auto operator[](const vector::size_type) const -> const any&;
  auto operator[](const any&) const -> const any&;

  auto contains(const char[]) const -> bool;

  auto extractPointer() const -> void*;

  static auto extractValue(const any&) -> const any&;

  DECLARE_COMPARISON_OPERATOR(==)
  DECLARE_COMPARISON_OPERATOR(!=)
  DECLARE_COMPARISON_OPERATOR(<)
  DECLARE_COMPARISON_OPERATOR(<=)
  DECLARE_COMPARISON_OPERATOR(>)
  DECLARE_COMPARISON_OPERATOR(>=)

  friend auto operator+(const any&, const any&) -> any;

  DECLARE_OPERATOR(+, long, long)
  DECLARE_OPERATOR(+, double, double)
  DECLARE_OPERATOR(+, char, char)
  DECLARE_OPERATOR(+, const char *, std::string)

  friend auto operator-(const any&, const any&) -> any;

  DECLARE_OPERATOR(-, long, long)
  DECLARE_OPERATOR(-, double, double)
  DECLARE_OPERATOR(-, char, char)

  friend auto operator*(const any&, const any&) -> any;

  DECLARE_OPERATOR(*, long, long)
  DECLARE_OPERATOR(*, double, double)
  DECLARE_OPERATOR(*, char, char)

  friend auto operator/(const any&, const any&) -> any;

  DECLARE_OPERATOR(/, long, long)
  DECLARE_OPERATOR(/, double, double)
  DECLARE_OPERATOR(/, char, char)

  friend auto operator%(const any&, const any&) -> any;

  DECLARE_OPERATOR(%, long, long)
  DECLARE_OPERATOR(%, char, char)

  friend auto operator-(const any&) -> any; // unary negate
};

} // namespace PureScript

#undef DECLARE_OPERATOR
#undef DECLARE_COMPARISON_OPERATOR

#endif // PureScript_HH
