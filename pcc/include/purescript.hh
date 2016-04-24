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
#include <deque>
#include <utility>
#include <stdexcept>
#include <iso646.h> // mostly for MS Visual Studio compiler

#define DEFINE_OPERATOR(op, ty, rty) \
  inline friend auto operator op (const any& lhs, ty rhs) -> rty { \
    return lhs.cast<ty>() op rhs; \
  } \
  inline friend auto operator op (ty lhs, const any& rhs) -> rty { \
    return lhs op rhs.cast<ty>(); \
  } \

#define DEFINE_INT_OPERATOR(op, rty) \
  inline friend auto operator op (const any& lhs, int rhs) -> rty { \
    return lhs.cast<long>() op (long)rhs; \
  } \
  inline friend auto operator op (int lhs, const any& rhs) -> rty { \
    return (long)lhs op rhs.cast<long>(); \
  }

#define DECLARE_COMPARISON_OPERATOR(op) \
  friend auto operator op (const any&, const any&) -> bool; \
  DEFINE_OPERATOR(op, long, bool) \
  DEFINE_OPERATOR(op, double, bool) \
  DEFINE_OPERATOR(op, char, bool) \
  DEFINE_INT_OPERATOR(op, bool) \
  friend auto operator op (const any&, const char * const) -> bool; \
  friend auto operator op (const char * const, const any&) -> bool;

namespace PureScript {

using string = const char *;
using nullptr_t = std::nullptr_t;

// Workaround for missing C++11 version in gcc
class runtime_error : public std::runtime_error {
public:
  runtime_error(const char message[]) : std::runtime_error(std::string(message)) {}
};

const bool undefined = false;
const size_t constructor = 0;

// A variant data class designed to provide some features of dynamic typing.
//
class any {

  public:
  enum Type {
    Thunk         = 0x00,
    Integer       = 0x01,
    Double        = 0x02,
    Character     = 0x03,
    Boolean       = 0x04,
    StringLiteral = 0x05,
    Function      = 0x06,
    Shared = 0x08,
    String        = 0x08,
    Map           = 0x09,
    Data          = 0x0A,
    Array         = 0x0B,
    Closure       = 0x0C,
    EffFunction   = 0x0D,
    Pointer       = 0x0E
  };

  private:
  mutable Type type;

  public:
  struct as_thunk {
  };
  static constexpr as_thunk unthunk = as_thunk{};

  using map     = std::vector<std::pair<const char * const, const any>>;
  using data    = std::vector<any>;
  using array   = std::deque<any>;
  using fn      = auto (*)(const any&) -> any;
  using thunk   = auto (*)(const as_thunk) -> const any&;

  private:
  class closure {
    public:
      virtual auto operator()(const any&) const -> any = 0;
      virtual ~closure() {}
  };

  template <typename T>
  class _closure : public closure {
    const T lambda;
  public:
    _closure(const T& l) noexcept : lambda(l) {}
    auto operator()(const any& arg) const -> any {
      return lambda(arg);
    }
  };

  class eff_fn {
    public:
      virtual auto operator()() const -> any = 0;
  };

  template <typename T>
  class _eff_fn : public eff_fn {
    const T lambda;
  public:
    _eff_fn(const T& l) noexcept : lambda(l) {}
    auto operator()() const -> any {
      return lambda();
    }
  };

  public:
  template <typename T>
  using shared = std::shared_ptr<T>;

  template <typename T, typename... Args>
  inline static auto make_shared(Args&&... args) -> decltype(std::make_shared<T>(std::forward<Args>(args)...)) {
    return std::make_shared<T>(std::forward<Args>(args)...);
  }

  private:
  union {
    mutable thunk                t;
    mutable long                 i;
    mutable double               d;
    mutable char                 c;
    mutable bool                 b;
    mutable string               r;
    mutable fn                   f;
    mutable shared<std::string>  s;
    mutable shared<map>          m;
    mutable shared<data>         v;
    mutable shared<array>        a;
    mutable shared<closure>      l;
    mutable shared<eff_fn>       e;
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

  any(const data& val) : type(Type::Data), v(make_shared<data>(val)) {}
  any(data&& val) noexcept : type(Type::Data), v(make_shared<data>(std::move(val))) {}

  any(const array& val) : type(Type::Array), a(make_shared<array>(val)) {}
  any(array&& val) noexcept : type(Type::Array), a(make_shared<array>(std::move(val))) {}

  template <typename T>
  any(const T& val, typename std::enable_if<std::is_convertible<T,fn>::value>::type* = 0)
    : type(Type::Function), f(val) {}

  template <typename T, typename = typename std::enable_if<!std::is_same<any,T>::value &&
                                                           !std::is_convertible<T,fn>::value>::type>
  any(const T& val, typename std::enable_if<std::is_assignable<std::function<any(const any&)>,T>::value>::type* = 0)
    : type(Type::Closure), l(make_shared<_closure<T>>(val)) {}

  template <typename T>
  any(const T& val, typename std::enable_if<std::is_assignable<std::function<any()>,T>::value>::type* = 0)
    : type(Type::EffFunction), e(make_shared<_eff_fn<T>>(val)) {}

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

  private:

  auto copy(const any& other) const -> void {
    if (type & Type::Shared) {
      new (&p) shared<void>(other.p);
    } else {
      f = other.f;
    }
  }

  auto move(any& other) const noexcept -> void {
    if (type & Type::Shared) {
      new (&p) shared<void>(std::move(other.p));
    } else {
      f = other.f;
    }
  }

  auto destruct() noexcept -> void {
    if (type & Type::Shared) {
      p.~shared<void>();
    }
  }

  public:

  any(const any& other) : type(other.type) {
    copy(other);
  }

  any(any&& other) noexcept : type(other.type) {
    move(other);
  }

  auto operator=(const any& rhs) -> any& {
    destruct();
    type = rhs.type;
    copy(rhs);
    return *this;
  }

  auto operator=(any&& rhs) noexcept -> any& {
    destruct();
    type = rhs.type;
    move(rhs);
    return *this;
  }

  any() = delete;

  ~any() {
    destruct();
  }

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
  auto cast() const -> typename std::enable_if<std::is_same<T, data>::value, const T&>::type;

  template <typename T>
  auto cast() const -> typename std::enable_if<std::is_same<T, array>::value, const T&>::type;

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
  operator const data&() const;
  operator const array&() const;

  auto operator[](const char[]) const -> const any&;
  auto operator[](const size_t) const -> const any&;
  auto operator[](const any&) const -> const any&;

  auto contains(const char[]) const -> bool;

  auto extractPointer() const -> void*;

  static auto unthunkVariant(const any&) -> const any&;

  DECLARE_COMPARISON_OPERATOR(==)
  DECLARE_COMPARISON_OPERATOR(!=)
  DECLARE_COMPARISON_OPERATOR(<)
  DECLARE_COMPARISON_OPERATOR(<=)
  DECLARE_COMPARISON_OPERATOR(>)
  DECLARE_COMPARISON_OPERATOR(>=)

  friend auto operator+(const any&, const any&) -> any;

  DEFINE_OPERATOR(+, long, long)
  DEFINE_OPERATOR(+, double, double)
  DEFINE_OPERATOR(+, char, char)
  DEFINE_INT_OPERATOR(+, long)
  friend auto operator+(const any& lhs, const char * const rhs) -> std::string;
  friend auto operator+(const char * const lhs, const any& rhs) -> std::string;

  friend auto operator-(const any&, const any&) -> any;

  DEFINE_OPERATOR(-, long, long)
  DEFINE_OPERATOR(-, double, double)
  DEFINE_OPERATOR(-, char, char)
  DEFINE_INT_OPERATOR(-, long)

  friend auto operator*(const any&, const any&) -> any;

  DEFINE_OPERATOR(*, long, long)
  DEFINE_OPERATOR(*, double, double)
  DEFINE_OPERATOR(*, char, char)
  DEFINE_INT_OPERATOR(*, long)

  friend auto operator/(const any&, const any&) -> any;

  DEFINE_OPERATOR(/, long, long)
  DEFINE_OPERATOR(/, double, double)
  DEFINE_OPERATOR(/, char, char)
  DEFINE_INT_OPERATOR(/, long)

  friend auto operator%(const any&, const any&) -> any;

  DEFINE_OPERATOR(%, long, long)
  DEFINE_OPERATOR(%, char, char)
  DEFINE_INT_OPERATOR(%, long)

  friend auto operator-(const any&) -> any; // unary negate
};

} // namespace PureScript

#undef DEFINE_OPERATOR
#undef DEFINE_INT_OPERATOR
#undef DECLARE_COMPARISON_OPERATOR

#endif // PureScript_HH
