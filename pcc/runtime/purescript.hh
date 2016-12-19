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

#if !defined(__cplusplus)
#error This is a C++ file!
#endif

// Standard includes

#if defined(DEBUG)
  #include <cassert>
  #include <limits>
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

namespace Private {
  struct symbol_generator_anchor_t {};

  template <typename T>
  struct symbol_generator {
    constexpr static symbol_generator_anchor_t anchor = symbol_generator_anchor_t{};
  };
  template <typename T>
  constexpr symbol_generator_anchor_t symbol_generator<T>::anchor;
}

using symbol_t = const Private::symbol_generator_anchor_t *;

#define define_symbol(S) namespace PureScript { \
                           namespace Private { \
                             namespace Symbol { \
                               struct S_ ## S {}; \
                             } \
                           } \
                         }
#define symbol(S) (&Private::symbol_generator<::PureScript::Private::Symbol::S_ ## S>::anchor)

// Workaround for missing C++11 version in gcc
class runtime_error : public std::runtime_error {
public:
  runtime_error(const char message[]) : std::runtime_error(std::string(message)) {}
};

constexpr bool undefined = false;

// Not a real limit, just used for simpler accessors
static constexpr size_t unknown_size = 64;

// A variant data class designed to provide some features of dynamic typing.
//
class any {

  public:
  enum class tag_t {
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
  mutable tag_t tag;

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
  class closure_ : public closure {
    const T lambda;
  public:
    closure_(const T& l) noexcept : lambda(l) {}
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
  class eff_closure_ : public eff_closure {
    const T lambda;
  public:
    eff_closure_(const T& l) noexcept : lambda(l) {}
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

  any(const int val) noexcept : tag(tag_t::Integer), i(val) {}
  any(const long val) noexcept : tag(tag_t::Integer), i(static_cast<decltype(i)>(val)) {
    assert(val >= std::numeric_limits<decltype(i)>::min() &&
           val <= std::numeric_limits<decltype(i)>::max());
  }

  any(const double val) noexcept : tag(tag_t::Double), d(val) {}
  any(const char val) noexcept : tag(tag_t::Character), c(val) {}

  template <typename T, typename = typename std::enable_if<std::is_same<bool,T>::value>::type>
  any(const T val) noexcept : tag(tag_t::Boolean), b(val) {}

  template <size_t N>
  any(const char (&val)[N]) noexcept : tag(tag_t::StringLiteral), r(val) {}
  any(char * val) : tag(tag_t::String), s(make_managed<std::string>(val)) {}

  any(const std::string& val) : tag(tag_t::String), s(make_managed<std::string>(val)) {}
  any(std::string&& val) noexcept : tag(tag_t::String), s(make_managed<std::string>(std::move(val))) {}

  any(const managed<std::string>& val) noexcept : tag(tag_t::String), s(val) {}
  any(managed<std::string>&& val) noexcept : tag(tag_t::String), s(std::move(val)) {}

  template <size_t N>
  any(map<N>&& val) noexcept : tag(tag_t::Map), p(make_managed<map<N>>(std::move(val))) {}

  template <size_t N>
  any(data<N>&& val) noexcept : tag(tag_t::Data), p(make_managed<data<N>>(std::move(val))) {}

  any(const array& val) : tag(tag_t::Array), a(make_managed<array>(val)) {}
  any(array&& val) noexcept : tag(tag_t::Array), a(make_managed<array>(std::move(val))) {}

  template <typename T>
  any(const T& val, typename std::enable_if<std::is_convertible<T,fn>::value>::type* = 0) noexcept
    : tag(tag_t::Function), f(val) {}

  template <typename T, typename = typename std::enable_if<!std::is_same<any,T>::value &&
                                                           !std::is_convertible<T,fn>::value>::type>
  any(const T& val, typename std::enable_if<std::is_assignable<std::function<any(const any&)>,T>::value>::type* = 0)
    : tag(tag_t::Closure), l(make_managed<closure_<T>>(val)) {}

  template <typename T>
  any(const T& val, typename std::enable_if<std::is_convertible<T,eff_fn>::value>::type* = 0) noexcept
    : tag(tag_t::EffFunction), e(val) {}

  template <typename T,
            typename = typename std::enable_if<!std::is_same<any,T>::value &&
                                               !std::is_convertible<T,eff_fn>::value>::type>
  any(const T& val, typename std::enable_if<std::is_assignable<std::function<any()>,T>::value>::type* = 0)
    : tag(tag_t::EffClosure), k(make_managed<eff_closure_<T>>(val)) {}

  template <typename T>
  any(const T& val, typename std::enable_if<std::is_convertible<T,thunk>::value>::type* = 0) noexcept
    : tag(tag_t::Thunk), t(val) {}

  template <typename T,
            typename = typename std::enable_if<std::is_class<typename std::remove_pointer<T>::type>::value>::type>
  any(const T& val, typename std::enable_if<IS_POINTER_TYPE(T)::value>::type* = 0) noexcept
    : tag(tag_t::Pointer), p(val) {}

  template <typename T>
  any(T&& val, typename std::enable_if<std::is_assignable<managed<void>,T>::value>::type* = 0) noexcept
    : tag(tag_t::Pointer), p(std::move(val)) {}

  // Explicit void* to raw pointer value
  template <typename T>
  any(const T& val, typename std::enable_if<std::is_same<T,void*>::value>::type* = 0) noexcept
    : tag(tag_t::RawPointer), u(val) {}

  any(nullptr_t) noexcept : tag(tag_t::RawPointer), u(nullptr) {}


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
    switch (other.tag) {
      case tag_t::Thunk:          t = other.t;  break;
      case tag_t::Integer:        i = other.i;  break;
      case tag_t::Double:         d = other.d;  break;
      case tag_t::Character:      c = other.c;  break;
      case tag_t::Boolean:        b = other.b;  break;
      case tag_t::StringLiteral:  r = other.r;  break;
      case tag_t::Function:       f = other.f;  break;
      case tag_t::EffFunction:    e = other.e;  break;
      case tag_t::RawPointer:     u = other.u;  break;

      case tag_t::String:      new (&s) managed<std::string>(move_if_rvalue<T>(other.s));  break;
      case tag_t::Array:       new (&a) managed<array>(move_if_rvalue<T>(other.a));        break;
      case tag_t::Closure:     new (&l) managed<closure>(move_if_rvalue<T>(other.l));      break;
      case tag_t::EffClosure:  new (&k) managed<eff_closure>(move_if_rvalue<T>(other.k));  break;
      case tag_t::Map:
      case tag_t::Data:
      case tag_t::Pointer:     new (&p) managed<void>(move_if_rvalue<T>(other.p));         break;

      default: assert(false && "Bad 'any' tag"); break;
    }
  }

  auto destruct() -> void {
    switch (tag) {
      case tag_t::String:      s.~managed<std::string>();   break;
      case tag_t::Array:       a.~managed<array>();         break;
      case tag_t::Closure:     l.~managed<closure>();       break;
      case tag_t::EffClosure:  k.~managed<eff_closure>();   break;
      case tag_t::Map:
      case tag_t::Data:
      case tag_t::Pointer:     p.~managed<void>();          break;

      default: break;
    }
  }

  public:
  any(const any& other) noexcept : tag(other.tag) {
    assign(other);
  }

  any(any&& other) noexcept : tag(other.tag) {
    assign(std::move(other));
  }

  auto operator=(const any& rhs) noexcept -> any& {
    destruct();
    tag = rhs.tag;
    assign(rhs);
    return *this;
  }

  auto operator=(any&& rhs) noexcept -> any& {
    destruct();
    tag = rhs.tag;
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

  auto size() const -> size_t;
  auto empty() const -> bool;
  auto contains(const symbol_t) const -> bool;

  auto extractPointer(IF_DEBUG(const tag_t)) const -> void*;

  auto rawPointer() const -> void* {
    const any& variant = unthunkVariant(*this);
    assert(tag == tag_t::RawPointer);
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

namespace Private {
  template <typename T, typename U=void>
  struct tag_helper_t {};

  template <size_t N>
  struct tag_helper_t<any::map<N>> {
    static constexpr any::tag_t tag = any::tag_t::Map;
  };

  template <size_t N>
  struct tag_helper_t<any::data<N>> {
    static constexpr any::tag_t tag = any::tag_t::Data;
  };

  template <typename T>
  struct tag_helper_t<T> {
    static constexpr any::tag_t tag = any::tag_t::Pointer;
  };
}

// Pass-through
template <typename T>
constexpr auto cast(const T& a) -> const T& {
  return a;
}

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
  return *static_cast<T*>(a.extractPointer(IF_DEBUG(Private::tag_helper_t<T>::tag)));
}

template <typename T>
inline auto cast(const any& a) ->
    typename std::enable_if<std::is_same<void*, T>::value, T>::type {
  return a.rawPointer();
}

namespace map {
  template <size_t N, typename T>
  inline auto get(const T& a) ->
      typename std::enable_if<!std::is_same<any, T>::value, const any&>::type {
    return a[N].second;
  }

  template <size_t N>
  inline auto get(const any& a) -> const any& {
    return cast<any::map<N+1>>(a)[N].second;
  }

  template <size_t N>
  inline auto get(const symbol_t key, const any::map<N>& a) -> const any& {
    static_assert(N > 0, "map size must be greater than zero");
    typename std::remove_reference<decltype(a)>::type::size_type i = 0;
    do {
      if (a[i].first == key) {
        return a[i].second;
      }
    } while (a[++i].first != nullptr);
    assert(false && "map key not found");
    static const any invalid_key(nullptr);
    return invalid_key;
  }

  inline auto get(const symbol_t key, const any& a) -> const any& {
    return get(key, cast<any::map<unknown_size>>(a));
  }
}

namespace data {
  template <size_t N, typename T>
  inline auto get(const T& a) ->
      typename std::enable_if<!std::is_same<any, T>::value, const any&>::type {
    return a[N];
  }

  template <size_t N>
  inline auto get(const any& a) -> const any& {
    return cast<any::data<N+1>>(a)[N];
  }

  template <typename T>
  inline auto ctor(const T& a) -> const any& {
    return get<0>(a);
  }
}

} // namespace PureScript

#undef WITH_ALLOCATOR
#undef IS_POINTER_TYPE
#undef DEFINE_OPERATOR
#undef DECLARE_COMPARISON_OPERATOR

#endif // PureScript_HH
