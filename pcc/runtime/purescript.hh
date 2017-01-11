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
using std::string;
using std::nullptr_t;
using std::runtime_error;

namespace Private {
  struct SymbolGeneratorAnchor {};

  template <typename T>
  struct SymbolGenerator {
    constexpr static SymbolGeneratorAnchor anchor = SymbolGeneratorAnchor{};
  };
  template <typename T>
  constexpr SymbolGeneratorAnchor SymbolGenerator<T>::anchor;

  using Symbol = const SymbolGeneratorAnchor *;
}

#define define_symbol(S) namespace PureScript { \
                           namespace Private { \
                             namespace Symbols { \
                               struct S_ ## S {}; \
                             } \
                           } \
                         }
#define symbol(S) (&Private::SymbolGenerator<::PureScript::Private::Symbols::S_ ## S>::anchor)

constexpr bool undefined = false;

// TODO: Not a real limit, just used for simpler accessors
static constexpr size_t unknown_size = 64;

// A variant data class designed to provide some features of dynamic typing.
//
class any {

  public:
  enum class Tag {
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
  mutable Tag tag;

  public:
  struct as_thunk {
  };
  static constexpr as_thunk unthunk = as_thunk{};

  using map_pair = std::pair<const Private::Symbol, const any>;

  template <size_t N>
  using map = std::array<const map_pair, N>;

  template <size_t N>
  using data = std::array<const any, N>;

  using array    = std::deque<any WITH_ALLOCATOR(any)>;
  using fn       = auto (*)(const any&) -> any;
  using eff_fn   = auto (*)() -> any;
  using thunk    = auto (*)(const as_thunk) -> const any&;

  private:
  class Closure {
    public:
      virtual auto operator()(const any&) const -> any = 0;
      virtual ~Closure();
  };

  template <typename T>
  class Closure_ : public Closure {
    const T lambda;
  public:
    Closure_(const T& l) noexcept : lambda(l) {}
    auto operator()(const any& arg) const -> any override {
      return lambda(arg);
    }
  };

  class EffClosure {
    public:
      virtual auto operator()() const -> any = 0;
      virtual ~EffClosure();
  };

  template <typename T>
  class EffClosure_ : public EffClosure {
    const T lambda;
  public:
    EffClosure_(const T& l) noexcept : lambda(l) {}
    auto operator()() const -> any override {
      return lambda();
    }
  };

  private:
  union {
    mutable thunk                 t;
    mutable int                   i;
    mutable double                d;
    mutable char32_t              c;
    mutable bool                  b;
    mutable cstring               r;
    mutable fn                    f;
    mutable eff_fn                e;
    mutable void *                v;
    mutable managed<string>       s;
    mutable managed<array>        a;
    mutable managed<Closure>      l;
    mutable managed<EffClosure>   k;
    mutable managed<void>         p;
  };

  public:

  any(const int val) noexcept : tag(Tag::Integer), i(val) {}
  any(const long val) noexcept : tag(Tag::Integer), i(static_cast<decltype(i)>(val)) {
    assert(val >= std::numeric_limits<decltype(i)>::min() &&
           val <= std::numeric_limits<decltype(i)>::max());
  }
  any(const double val) noexcept : tag(Tag::Double), d(val) {}
  any(const char32_t val) noexcept : tag(Tag::Character), c(val) {}
  any(const char val) noexcept : tag(Tag::Character), c(val) {}

  template <typename T, typename = typename std::enable_if<std::is_same<bool,T>::value>::type>
  any(const T val) noexcept : tag(Tag::Boolean), b(val) {}

  any(const char * val) : tag(Tag::String), s(make_managed<string>(val)) {}
  any(const string& val) : tag(Tag::String), s(make_managed<string>(val)) {}
  any(string&& val) noexcept : tag(Tag::String), s(make_managed<string>(std::move(val))) {}

  any(const managed<string>& val) noexcept : tag(Tag::String), s(val) {}
  any(managed<string>&& val) noexcept : tag(Tag::String), s(std::move(val)) {}

  template <size_t N>
  any(map<N>&& val) noexcept : tag(Tag::Map), p(make_managed<map<N>>(std::move(val))) {}

  template <size_t N>
  any(data<N>&& val) noexcept : tag(Tag::Data), p(make_managed<data<N>>(std::move(val))) {}

  any(const array& val) : tag(Tag::Array), a(make_managed<array>(val)) {}
  any(array&& val) noexcept : tag(Tag::Array), a(make_managed<array>(std::move(val))) {}

  template <typename T>
  any(const T& val, typename std::enable_if<std::is_convertible<T,fn>::value>::type* = 0) noexcept
    : tag(Tag::Function), f(val) {}

  template <typename T, typename = typename std::enable_if<!std::is_same<any,T>::value &&
                                                           !std::is_convertible<T,fn>::value>::type>
  any(const T& val, typename std::enable_if<std::is_assignable<std::function<any(const any&)>,T>::value>::type* = 0)
    : tag(Tag::Closure), l(make_managed<Closure_<T>>(val)) {}

  template <typename T>
  any(const T& val, typename std::enable_if<std::is_convertible<T,eff_fn>::value>::type* = 0) noexcept
    : tag(Tag::EffFunction), e(val) {}

  template <typename T,
            typename = typename std::enable_if<!std::is_same<any,T>::value &&
                                               !std::is_convertible<T,eff_fn>::value>::type>
  any(const T& val, typename std::enable_if<std::is_assignable<std::function<any()>,T>::value>::type* = 0)
    : tag(Tag::EffClosure), k(make_managed<EffClosure_<T>>(val)) {}

  template <typename T>
  any(const T& val, typename std::enable_if<std::is_convertible<T,thunk>::value>::type* = 0) noexcept
    : tag(Tag::Thunk), t(val) {}

  template <typename T,
            typename = typename std::enable_if<std::is_class<typename std::remove_pointer<T>::type>::value>::type>
  any(const T& val, typename std::enable_if<IS_POINTER_TYPE(T)::value>::type* = 0) noexcept
    : tag(Tag::Pointer), p(val) {}

  template <typename T>
  any(T&& val, typename std::enable_if<std::is_assignable<managed<void>,T>::value>::type* = 0) noexcept
    : tag(Tag::Pointer), p(std::move(val)) {}

  // Explicit void* to raw pointer value
  template <typename T>
  any(const T& val, typename std::enable_if<std::is_same<T,void*>::value>::type* = 0) noexcept
    : tag(Tag::RawPointer), v(val) {}

  any(nullptr_t) noexcept : tag(Tag::RawPointer), v(nullptr) {}


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
      case Tag::Thunk:          t = other.t;  break;
      case Tag::Integer:        i = other.i;  break;
      case Tag::Double:         d = other.d;  break;
      case Tag::Character:      c = other.c;  break;
      case Tag::Boolean:        b = other.b;  break;
      case Tag::StringLiteral:  r = other.r;  break;
      case Tag::Function:       f = other.f;  break;
      case Tag::EffFunction:    e = other.e;  break;
      case Tag::RawPointer:     v = other.v;  break;

      case Tag::String:      new (&s) managed<string>(move_if_rvalue<T>(other.s));      break;
      case Tag::Array:       new (&a) managed<array>(move_if_rvalue<T>(other.a));       break;
      case Tag::Closure:     new (&l) managed<Closure>(move_if_rvalue<T>(other.l));     break;
      case Tag::EffClosure:  new (&k) managed<EffClosure>(move_if_rvalue<T>(other.k));  break;
      case Tag::Map:
      case Tag::Data:
      case Tag::Pointer:     new (&p) managed<void>(move_if_rvalue<T>(other.p));        break;

      default: assert(false && "Bad 'any' tag"); break;
    }
  }

  auto destruct() -> void {
    switch (tag) {
      case Tag::String:      s.~managed<string>();      break;
      case Tag::Array:       a.~managed<array>();       break;
      case Tag::Closure:     l.~managed<Closure>();     break;
      case Tag::EffClosure:  k.~managed<EffClosure>();  break;
      case Tag::Map:
      case Tag::Data:
      case Tag::Pointer:     p.~managed<void>();        break;

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
  operator char32_t() const;
  operator char() const;
  operator size_t() const;
  operator const char *() const;
  operator const array&() const;

  auto operator[](const size_t) const -> const any&;

  auto size() const -> size_t;
  auto empty() const -> bool;
  auto contains(const Private::Symbol) const -> bool;

  auto extractPointer(IF_DEBUG(const Tag)) const -> void*;

  auto rawPointer() const -> void* {
    const any& variant = unthunkVariant(*this);
    assert(tag == Tag::RawPointer);
    return variant.v;
  }

  static auto unthunkVariant(const any&) -> const any&;

  #define DEFINE_OPERATOR(op) \
    template <typename T> \
    inline friend auto operator op (const any& lhs, T rhs) -> T { \
      return static_cast<T>(lhs) op rhs; \
    } \
    template <typename T> \
    inline friend auto operator op (T lhs, const any& rhs) -> T { \
      return lhs op static_cast<T>(rhs); \
    } \
    friend auto operator op (const any&, const any&) -> any;

  #define DEFINE_COMPARISON_OPERATOR(op) \
    template <typename T> \
    inline friend auto operator op (const any& lhs, T rhs) -> bool { \
      return static_cast<T>(lhs) op rhs; \
    } \
    template <typename T> \
    inline friend auto operator op (T lhs, const any& rhs) -> bool { \
      return lhs op static_cast<T>(rhs); \
    } \
    friend auto operator op (const any&, const any&) -> bool; \
    friend auto operator op (const any&, const char * const) -> bool; \
    friend auto operator op (const char * const, const any&) -> bool;

  DEFINE_COMPARISON_OPERATOR(==)
  DEFINE_COMPARISON_OPERATOR(!=)
  DEFINE_COMPARISON_OPERATOR(<)
  DEFINE_COMPARISON_OPERATOR(<=)
  DEFINE_COMPARISON_OPERATOR(>)
  DEFINE_COMPARISON_OPERATOR(>=)

  DEFINE_OPERATOR(+)
  DEFINE_OPERATOR(-)
  DEFINE_OPERATOR(*)
  DEFINE_OPERATOR(/)
  DEFINE_OPERATOR(%)

  friend auto operator+(const any& lhs, const char * const rhs) -> string;
  friend auto operator+(const char * const lhs, const any& rhs) -> string;

  friend auto operator-(const any&) -> any; // unary negate

}; // class any

namespace Private {
  template <typename T, typename U=void>
  struct TagHelper {};

  template <size_t N>
  struct TagHelper<any::map<N>> {
    static constexpr any::Tag tag = any::Tag::Map;
  };

  template <size_t N>
  struct TagHelper<any::data<N>> {
    static constexpr any::Tag tag = any::Tag::Data;
  };

  template <typename T>
  struct TagHelper<T> {
    static constexpr any::Tag tag = any::Tag::Pointer;
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
                            std::is_same<T,const char *>::value, T>::type {
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
  return *static_cast<T*>(a.extractPointer(IF_DEBUG(Private::TagHelper<T>::tag)));
}

template <typename T,
          typename = typename std::enable_if<std::is_pointer<T>::value &&
                                            !std::is_same<T,void*>::value &&
                                            !std::is_same<T,const char *>::value>::type>
inline auto cast(const any& a) -> T {
  return static_cast<T>(a.extractPointer(IF_DEBUG(Private::TagHelper<T>::tag)));
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
  inline auto get(const Private::Symbol key, const any::map<N>& a) -> const any& {
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

  inline auto get(const Private::Symbol key, const any& a) -> const any& {
    return get(key, cast<any::map<unknown_size>>(a));
  }

  #define P(N)    any::map_pair&& arg ## N
  #define T(N)    any::map<N+1>
  #define E(N)    std::forward<decltype(arg ## N)>(arg ## N)
  #define M(...)  {{ __VA_ARGS__, { nullptr, nullptr } }}

  inline auto make(P(1)) -> T(1) {
    return T(1) M(E(1));
  }
  inline auto make(P(1), P(2)) -> T(2) {
    return T(2) M(E(1), E(2));
  }
  inline auto make(P(1), P(2), P(3)) -> T(3) {
    return T(3) M(E(1), E(2), E(3));
  }
  inline auto make(P(1), P(2), P(3), P(4)) -> T(4) {
    return T(4) M(E(1), E(2), E(3), E(4));
  }
  inline auto make(P(1), P(2), P(3), P(4), P(5)) -> T(5) {
    return T(5) M(E(1), E(2), E(3), E(4), E(5));
  }
  inline auto make(P(1), P(2), P(3), P(4), P(5), P(6)) -> T(6) {
    return T(6) M(E(1), E(2), E(3), E(4), E(5), E(6));
  }
  inline auto make(P(1), P(2), P(3), P(4), P(5), P(6), P(7)) -> T(7) {
    return T(7) M(E(1), E(2), E(3), E(4), E(5), E(6), E(7));
  }
  inline auto make(P(1), P(2), P(3), P(4), P(5), P(6), P(7), P(8)) -> T(8) {
    return T(8) M(E(1), E(2), E(3), E(4), E(5), E(6), E(7), E(8));
  }

  #undef P
  #undef T
  #undef E
  #undef M
}

namespace record = map;

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
  inline auto ctor(const T& a) -> int {
    return get<0>(a);
  }
}

} // namespace PureScript

#undef WITH_ALLOCATOR
#undef IS_POINTER_TYPE
#undef DEFINE_OPERATOR
#undef DEFINE_COMPARISON_OPERATOR

#endif // PureScript_HH
