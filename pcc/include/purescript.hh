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
  #include <climits>
  #define IF_DEBUG(x) x
#else
  #define NDEBUG
  #undef assert
  #define assert(x)
  #define IF_DEBUG(_)
#endif

#include <functional>
#include <string>
#include <array>
#include <deque>
#include <utility>
#include <stdexcept>
#include <iso646.h> // mostly for MS Visual Studio compiler
#include "purescript_memory.hh"

namespace PureScript {

using cstring = const char *;
using std::nullptr_t;

struct symbol_generator_anchor {};
using symbol_t = const symbol_generator_anchor *;

template <typename T>
struct symbol_generator {
  constexpr static symbol_generator_anchor anchor = symbol_generator_anchor{};
};
template <typename T>
constexpr symbol_generator_anchor symbol_generator<T>::anchor;

#define SYMBOL(S) &symbol_generator<symbol::S>::anchor

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
  enum class Type {
    Thunk = 0x10,
    Integer,
    Double,
    Character,
    Boolean,
    StringLiteral,
    Function,
    EffFunction,
    RawPointer,
    String,
    Map,
    Data,
    Array,
    Closure,
    EffClosure,
    Pointer
  };

  private:
  mutable Type type;

  public:
  struct as_thunk {
  };
  static constexpr as_thunk unthunk = as_thunk{};

  using map_pair = std::pair<const symbol_t, const any>;

  template <size_t N>
  using map = std::array<const map_pair, N>;

  template <size_t N>
  using data = std::array<const any, N>;

  using array    = std::deque<any WITH_ALLOCATOR(any)>;
  using fn       = auto (*)(const any&) -> any;
  using eff_fn   = auto (*)() -> any;
  using thunk    = auto (*)(const as_thunk) -> const any&;

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
    auto operator()(const any& arg) const -> any override {
      return lambda(arg);
    }
  };

  class eff_closure {
    public:
      virtual auto operator()() const -> any = 0;
      virtual ~eff_closure() {}
  };

  template <typename T>
  class _eff_closure : public eff_closure {
    const T lambda;
  public:
    _eff_closure(const T& l) noexcept : lambda(l) {}
    auto operator()() const -> any override {
      return lambda();
    }
  };

  private:
  union {
    mutable thunk                 t;
    mutable int                   i;
    mutable double                d;
    mutable char                  c;
    mutable bool                  b;
    mutable cstring               r;
    mutable fn                    f;
    mutable eff_fn                e;
    mutable void *                u;
    mutable managed<std::string>  s;
    mutable managed<array>        a;
    mutable managed<closure>      l;
    mutable managed<eff_closure>  k;
    mutable managed<void>         p;
  };

  public:

  any(const int val) noexcept : type(Type::Integer), i(val) {}
  any(const long int_value) noexcept : type(Type::Integer), i(int_value) {
    assert(int_value >= INT_MIN && int_value <= INT_MAX);
  }

  any(const double val) noexcept : type(Type::Double), d(val) {}
  any(const char val) noexcept : type(Type::Character), c(val) {}

  template <typename T, typename = typename std::enable_if<std::is_same<bool,T>::value>::type>
  any(const T val) noexcept : type(Type::Boolean), b(val) {}

  template <size_t N>
  any(const char (&val)[N]) noexcept : type(Type::StringLiteral), r(val) {}
  any(char * val) : type(Type::String), s(make_managed<std::string>(val)) {}

  any(const std::string& val) : type(Type::String), s(make_managed<std::string>(val)) {}
  any(std::string&& val) noexcept : type(Type::String), s(make_managed<std::string>(std::move(val))) {}

  any(const managed<std::string>& val) noexcept : type(Type::String), s(val) {}
  any(managed<std::string>&& val) noexcept : type(Type::String), s(std::move(val)) {}

  template <size_t N>
  any(map<N>&& val) noexcept : type(Type::Map), p(make_managed<map<N>>(std::move(val))) {}

  template <size_t N>
  any(data<N>&& val) noexcept : type(Type::Data), p(make_managed<data<N>>(std::move(val))) {}

  any(const array& val) : type(Type::Array), a(make_managed<array>(val)) {}
  any(array&& val) noexcept : type(Type::Array), a(make_managed<array>(std::move(val))) {}

  template <typename T>
  any(const T& val, typename std::enable_if<std::is_convertible<T,fn>::value>::type* = 0) noexcept
    : type(Type::Function), f(val) {}

  template <typename T, typename = typename std::enable_if<!std::is_same<any,T>::value &&
                                                           !std::is_convertible<T,fn>::value>::type>
  any(const T& val, typename std::enable_if<std::is_assignable<std::function<any(const any&)>,T>::value>::type* = 0)
    : type(Type::Closure), l(make_managed<_closure<T>>(val)) {}

  template <typename T>
  any(const T& val, typename std::enable_if<std::is_convertible<T,eff_fn>::value>::type* = 0) noexcept
    : type(Type::EffFunction), e(val) {}

  template <typename T,
            typename = typename std::enable_if<!std::is_same<any,T>::value &&
                                               !std::is_convertible<T,eff_fn>::value>::type>
  any(const T& val, typename std::enable_if<std::is_assignable<std::function<any()>,T>::value>::type* = 0)
    : type(Type::EffClosure), k(make_managed<_eff_closure<T>>(val)) {}

  template <typename T>
  any(const T& val, typename std::enable_if<std::is_convertible<T,thunk>::value>::type* = 0) noexcept
    : type(Type::Thunk), t(val) {}

  template <typename T,
            typename = typename std::enable_if<std::is_class<typename std::remove_pointer<T>::type>::value>::type>
  any(const T& val, typename std::enable_if<IS_POINTER_TYPE(T)::value>::type* = 0) noexcept
    : type(Type::Pointer), p(val) {}

  template <typename T>
  any(T&& val, typename std::enable_if<std::is_assignable<managed<void>,T>::value>::type* = 0) noexcept
    : type(Type::Pointer), p(std::move(val)) {}

  // Explicit void* to raw pointer value
  template <typename T>
  any(const T& val, typename std::enable_if<std::is_same<T,void*>::value>::type* = 0) noexcept
    : type(Type::RawPointer), u(val) {}

  any(std::nullptr_t) noexcept : type(Type::RawPointer), u(nullptr) {}


#if !defined(USE_GC)
  private:
  template <typename T,
            typename U,
            typename = typename std::enable_if<!std::is_reference<T>::value>::type>
  static constexpr auto move_if_rvalue(U&& u) noexcept -> U&& {
    return static_cast<U&&>(u);
  }

  template <typename T,
            typename U,
            typename = typename std::enable_if<std::is_reference<T>::value>::type>
  static constexpr auto move_if_rvalue(U& u) noexcept -> U& {
    return static_cast<U&>(u);
  }

  template <typename T>
  auto assign(T&& other) const noexcept -> void {
    switch (other.type) {
      case Type::Thunk:          t = other.t;  break;
      case Type::Integer:        i = other.i;  break;
      case Type::Double:         d = other.d;  break;
      case Type::Character:      c = other.c;  break;
      case Type::Boolean:        b = other.b;  break;
      case Type::StringLiteral:  r = other.r;  break;
      case Type::Function:       f = other.f;  break;
      case Type::EffFunction:    e = other.e;  break;
      case Type::RawPointer:     u = other.u;  break;

      case Type::String:      new (&s) managed<std::string>(move_if_rvalue<T>(other.s));  break;
      case Type::Array:       new (&a) managed<array>(move_if_rvalue<T>(other.a));        break;
      case Type::Closure:     new (&l) managed<closure>(move_if_rvalue<T>(other.l));      break;
      case Type::EffClosure:  new (&k) managed<eff_closure>(move_if_rvalue<T>(other.k));  break;
      case Type::Map:
      case Type::Data:
      case Type::Pointer:     new (&p) managed<void>(move_if_rvalue<T>(other.p));         break;

      default: assert(false && "Bad 'any' type"); break;
    }
  }

  auto destruct() -> void {
    switch (type) {
      case Type::String:      s.~managed<std::string>();   break;
      case Type::Array:       a.~managed<array>();         break;
      case Type::Closure:     l.~managed<closure>();       break;
      case Type::EffClosure:  k.~managed<eff_closure>();   break;
      case Type::Map:
      case Type::Data:
      case Type::Pointer:     p.~managed<void>();          break;

      default: break;
    }
  }

  public:
  any(const any& other) noexcept : type(other.type) {
    assign(other);
  }

  any(any&& other) noexcept : type(other.type) {
    assign(std::move(other));
  }

  auto operator=(const any& rhs) noexcept -> any& {
    destruct();
    type = rhs.type;
    assign(rhs);
    return *this;
  }

  auto operator=(any&& rhs) noexcept -> any& {
    destruct();
    type = rhs.type;
    assign(std::move(rhs));
    return *this;
  }

  ~any() {
    destruct();
  }
#endif // !defined(USE_GC)

  public:
  any() = delete;

  auto operator()(const any&) const -> any;
  auto operator()(const as_thunk) const -> const any&;
  auto operator()() const -> any;

  operator int() const;
  operator double() const;
  operator bool() const;
  operator char() const;
  operator cstring() const;
  operator const array&() const;

  auto operator[](const symbol_t) const -> const any&;
  auto operator[](const size_t) const -> const any&;
  auto operator[](const any&) const -> const any&;

  auto contains(const symbol_t) const -> bool;

  auto extractPointer(IF_DEBUG(const Type)) const -> void*;

  auto rawPointer() const -> void* {
    const any& variant = unthunkVariant(*this);
    assert(type == Type::RawPointer);
    return variant.u;
  }

  static auto unthunkVariant(const any&) -> const any&;

  #define DEFINE_OPERATOR(op, ty, rty) \
    inline friend auto operator op (const any& lhs, ty rhs) -> rty { \
      return (ty)lhs op rhs; \
    } \
    inline friend auto operator op (ty lhs, const any& rhs) -> rty { \
      return lhs op (ty)rhs; \
    } \

  #define DECLARE_COMPARISON_OPERATOR(op) \
    friend auto operator op (const any&, const any&) -> bool; \
    DEFINE_OPERATOR(op, int, bool) \
    DEFINE_OPERATOR(op, double, bool) \
    DEFINE_OPERATOR(op, char, bool) \
    DEFINE_OPERATOR(op, bool, bool) \
    friend auto operator op (const any&, const char * const) -> bool; \
    friend auto operator op (const char * const, const any&) -> bool;

  DECLARE_COMPARISON_OPERATOR(==)
  DECLARE_COMPARISON_OPERATOR(!=)
  DECLARE_COMPARISON_OPERATOR(<)
  DECLARE_COMPARISON_OPERATOR(<=)
  DECLARE_COMPARISON_OPERATOR(>)
  DECLARE_COMPARISON_OPERATOR(>=)

  friend auto operator+(const any&, const any&) -> any;

  DEFINE_OPERATOR(+, int, int)
  DEFINE_OPERATOR(+, double, double)
  DEFINE_OPERATOR(+, char, char)
  friend auto operator+(const any& lhs, const char * const rhs) -> std::string;
  friend auto operator+(const char * const lhs, const any& rhs) -> std::string;

  friend auto operator-(const any&, const any&) -> any;

  DEFINE_OPERATOR(-, int, int)
  DEFINE_OPERATOR(-, double, double)
  DEFINE_OPERATOR(-, char, char)

  friend auto operator*(const any&, const any&) -> any;

  DEFINE_OPERATOR(*, int, int)
  DEFINE_OPERATOR(*, double, double)
  DEFINE_OPERATOR(*, char, char)

  friend auto operator/(const any&, const any&) -> any;

  DEFINE_OPERATOR(/, int, int)
  DEFINE_OPERATOR(/, double, double)
  DEFINE_OPERATOR(/, char, char)

  friend auto operator%(const any&, const any&) -> any;

  DEFINE_OPERATOR(%, int, int)
  DEFINE_OPERATOR(%, char, char)

  friend auto operator-(const any&) -> any; // unary negate

}; // class any

template <typename T, typename U=void>
struct tag_helper {};

template <size_t N>
struct tag_helper<any::map<N>> {
  static constexpr any::Type tag = any::Type::Map;
};

template <size_t N>
struct tag_helper<any::data<N>> {
  static constexpr any::Type tag = any::Type::Data;
};

template <typename T>
struct tag_helper<T> {
  static constexpr any::Type tag = any::Type::Pointer;
};

template <typename T>
inline auto cast(const any& a) ->
    typename std::enable_if<std::is_arithmetic<T>::value ||
                            std::is_same<T,cstring>::value, T>::type {
  return a;
}

template <typename T>
inline auto cast(const any& a) ->
    typename std::enable_if<std::is_same<T, any::array>::value, const T&>::type {
  return a;
}

template <typename T, typename = typename std::enable_if<std::is_class<T>::value>::type>
inline auto cast(const any& a) ->
    typename std::enable_if<!std::is_same<T, any::array>::value, T&>::type {
  return *static_cast<T*>(a.extractPointer(IF_DEBUG(tag_helper<T>::tag)));
}

template <typename T>
inline auto cast(const any& a) ->
    typename std::enable_if<std::is_same<void*, T>::value, T>::type {
  return a.rawPointer();
}

} // namespace PureScript

#undef WITH_ALLOCATOR
#undef IS_POINTER_TYPE
#undef DEFINE_OPERATOR
#undef DECLARE_COMPARISON_OPERATOR

#endif // PureScript_HH
