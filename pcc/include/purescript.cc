///////////////////////////////////////////////////////////////////////////////
//
// Module      :  purescript.cc
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
#include <cstring>
#include "PureScript.hh"

namespace PureScript {

#define COPY_DATA(src) \
  switch (type) { \
    case Type::Integer:         i = src.i;                           break; \
    case Type::Double:          d = src.d;                           break; \
    case Type::Character:       c = src.c;                           break; \
    case Type::Boolean:         b = src.b;                           break; \
    case Type::StringLiteral:   r = src.r;                           break; \
    case Type::String:          new (&s) shared<std::string>(src.s); break; \
    case Type::Map:             new (&m) shared<map>(src.m);         break; \
    case Type::Vector:          new (&v) shared<vector>(src.v);      break; \
    case Type::Array:           new (&a) shared<array>(src.a);       break; \
    case Type::Function:        f = src.f;                           break; \
    case Type::Closure:         new (&l) shared<closure>(src.l);     break; \
    case Type::EffFunction:     new (&e) shared<eff_fn>(src.e);      break; \
    case Type::Thunk:           t = src.t;                           break; \
    case Type::Pointer:         new (&p) shared<void>(src.p);        break; \
    \
    default: assert(false && "Unsupported type in copy"); \
  }

#define MOVE_DATA(src) \
  switch (type) { \
    case Type::Integer:         i = src.i;                                      break; \
    case Type::Double:          d = src.d;                                      break; \
    case Type::Character:       c = src.c;                                      break; \
    case Type::Boolean:         b = src.b;                                      break; \
    case Type::StringLiteral:   r = src.r;                                      break; \
    case Type::String:          new (&s) shared<std::string>(std::move(src.s)); break; \
    case Type::Map:             new (&m) shared<map>(std::move(src.m));         break; \
    case Type::Vector:          new (&v) shared<vector>(std::move(src.v));      break; \
    case Type::Array:           new (&a) shared<array>(std::move(src.a));       break; \
    case Type::Function:        f = src.f;                                      break; \
    case Type::Closure:         new (&l) shared<closure>(std::move(src.l));     break; \
    case Type::EffFunction:     new (&e) shared<eff_fn>(std::move(src.e));      break; \
    case Type::Thunk:           t = src.t;                                      break; \
    case Type::Pointer:         new (&p) shared<void>(std::move(src.p));        break; \
    \
    default: assert(false && "Unsupported type in move"); \
  }

any::any(const any& other) : type(other.type) {
  COPY_DATA(other);
}

any::any(any&& other) noexcept : type(other.type) {
  MOVE_DATA(other);
}

auto any::operator=(const any& rhs) -> any& {
  type = rhs.type;
  COPY_DATA(rhs);
  return *this;
}

// Takes ownership -- might need to reconsider this
auto any::operator=(any& rhs) noexcept -> any& {
  type = rhs.type;
  MOVE_DATA(rhs);
  return *this;
}

auto any::operator=(any&& rhs) noexcept -> any& {
  type = rhs.type;
  MOVE_DATA(rhs);
  return *this;
}

any::~any() {
  switch (type) {
    case Type::Integer:         ;                          break;
    case Type::Double:          ;                          break;
    case Type::Character:       ;                          break;
    case Type::Boolean:         ;                          break;
    case Type::StringLiteral:   ;                          break;
    case Type::String:          s.~shared<std::string>();  break;
    case Type::Map:             m.~shared<map>();          break;
    case Type::Vector:          v.~shared<vector>();       break;
    case Type::Array:           a.~shared<array>();        break;
    case Type::Function:        ;                          break;
    case Type::Closure:         l.~shared<closure>();      break;
    case Type::EffFunction:     e.~shared<eff_fn>();       break;
    case Type::Thunk:           ;                          break;
    case Type::Pointer:         p.~shared<void>();         break;

    default: assert(false && "Unsupported type in destructor");
  }
};

#define RETURN_VALUE(TYPE, ACCESSOR, INDIRECTION) \
  const any& variant = unthunkVariant(*this); \
  assert(variant.type == TYPE); \
  return INDIRECTION(variant.ACCESSOR); \

template <typename T>
auto any::cast() const -> typename std::enable_if<std::is_same<T, long>::value, T>::type {
  RETURN_VALUE(Type::Integer, i,)
}
template auto any::cast<long>() const -> long;

template <typename T>
auto any::cast() const -> typename std::enable_if<std::is_same<T, double>::value, T>::type {
  RETURN_VALUE(Type::Double, d,)
}
template auto any::cast<double>() const -> double;

template <typename T>
auto any::cast() const -> typename std::enable_if<std::is_same<T, char>::value, T>::type {
  RETURN_VALUE(Type::Character, c,)
}
template auto any::cast<char>() const -> char;

template <typename T>
auto any::cast() const -> typename std::enable_if<std::is_same<T, bool>::value, T>::type {
  RETURN_VALUE(Type::Boolean, b,)
}
template auto any::cast<bool>() const -> bool;

template <typename T>
auto any::cast() const -> typename std::enable_if<std::is_same<T, string>::value, T>::type {
  const any& variant = unthunkVariant(*this);
  if (variant.type == Type::StringLiteral) {
    return variant.r;
  }
  assert(variant.type == Type::String);
  return variant.s->c_str();
}
template auto any::cast<string>() const -> string;

template <typename T>
auto any::cast() const -> typename std::enable_if<std::is_same<T, map>::value, const T&>::type {
  RETURN_VALUE(Type::Map, m, *)
}
template auto any::cast<any::map>() const -> const map&;

template <typename T>
auto any::cast() const -> typename std::enable_if<std::is_same<T, vector>::value, const T&>::type {
  RETURN_VALUE(Type::Vector, v, *)
}
template auto any::cast<any::vector>() const -> const vector&;

template <typename T>
auto any::cast() const -> typename std::enable_if<std::is_same<T, array>::value, const T&>::type {
  RETURN_VALUE(Type::Array, a, *)
}
template auto any::cast<any::array>() const -> const array&;

auto any::operator()(const any& arg) const -> any {
  const any& variant = unthunkVariant(*this);
  if (variant.type == Type::Closure) {
    return (*variant.l)(arg);
  }
  assert(variant.type == Type::Function);
  return (*variant.f)(arg);
}

auto any::operator()(const as_thunk) const -> const any& {
  assert(type == Type::Thunk);
  return (*t)(unthunk);
}

auto any::operator()() const -> any {
  const any& variant = unthunkVariant(*this);
  assert(variant.type == Type::EffFunction);
  return (*variant.e)();
}

any::operator long() const {
  return cast<long>();
}

any::operator double() const {
  return cast<double>();
}

any::operator bool() const {
  return cast<bool>();
}

any::operator string() const {
  return cast<string>();
}

any::operator const map&() const {
  return cast<map>();
}

any::operator const vector&() const {
  return cast<vector>();
}

any::operator const array&() const {
  return cast<array>();
}

auto any::extractPointer() const -> void* {
  RETURN_VALUE(Type::Pointer, p.get(),)
}

auto any::unthunkVariant(const any& a) -> const any& {
  const any * variant = &a;
  while (variant->type == Type::Thunk) {
    variant = &((*variant->t)(unthunk));
  }
  return *variant;
}

auto any::operator[](const char rhs[]) const -> const any& {
  const any& variant = unthunkVariant(*this);
  assert(variant.type == Type::Map);
  const any::map::const_iterator begin = variant.m->begin();
  const any::map::const_iterator end = variant.m->end();
  for (any::map::const_iterator it = begin; it != end; ++it) {
    if (it->first == rhs) {
      return it->second;
    }
  }
  for (any::map::const_iterator it = begin; it != end; ++it) {
    if (strcmp(it->first, rhs) == 0) {
      return it->second;
    }
  }
  throw runtime_error("map key not found");
}

auto any::operator[](const size_t rhs) const -> const any& {
  const any& variant = unthunkVariant(*this);
  assert(variant.type == Type::Array);
  return (*variant.a)[rhs];
}

auto any::operator[](const any& rhs) const -> const any& {
  const any& variant = unthunkVariant(*this);
  assert(variant.type == Type::Array);
  return (*variant.a)[rhs.cast<long>()];
}

auto any::contains(const char key[]) const -> bool {
  const any& variant = unthunkVariant(*this);
  assert(variant.type == Type::Map);
  for (any::map::const_iterator it = variant.m->begin(), end = variant.m->end(); it != end; ++it) {
    if (it->first == key || strcmp(it->first, key) == 0) {
      return true;
    }
  }
  return false;
}

//-----------------------------------------------------------------------------
// Operator helper macros
//-----------------------------------------------------------------------------

#define DEFINE_OPERATOR(op, ty, rty) \
auto operator op (const any& lhs, ty rhs) -> rty { \
  return lhs.cast<typename std::remove_const<std::remove_reference<ty>::type>::type>() op rhs; \
} \
auto operator op (ty lhs, const any& rhs) -> rty { \
  return lhs op rhs.cast<typename std::remove_const<std::remove_reference<ty>::type>::type>(); \
}

#define DEFINE_CSTR_EQUALS_OPERATOR() \
  auto operator==(const any& lhs_, const char * rhs) -> bool { \
    const any& lhs = any::unthunkVariant(lhs_); \
    assert(lhs.type == any::Type::StringLiteral || lhs.type == any::Type::String); \
    if (lhs.type == any::Type::StringLiteral) { \
      return (lhs.r == rhs) || (strcmp(lhs.r, rhs) == 0); \
    } \
    return strcmp(lhs.s->c_str(), rhs) == 0; \
  } \
  auto operator==(const char * lhs, const any& rhs_) -> bool { \
    const any& rhs = any::unthunkVariant(rhs_); \
    assert(rhs.type == any::Type::StringLiteral || rhs.type == any::Type::String); \
    if (rhs.type == any::Type::StringLiteral) { \
      return (lhs == rhs.r) || (strcmp(lhs, rhs) == 0); \
    } \
    return strcmp(lhs, rhs.s->c_str()) == 0; \
  }

#define DEFINE_CSTR_COMPARISON_OPERATOR(op) \
  auto operator op (const any& lhs, const char * rhs) -> bool { \
    return strcmp(lhs.cast<string>(), rhs) op 0; \
  } \
  auto operator op (const char * lhs, const any& rhs) -> bool { \
    return strcmp(lhs, rhs.cast<string>()) op 0; \
  }

#define DEFINE_COMPARISON_OPERATOR(op) \
  auto operator op (const any& lhs_, const any& rhs_) -> bool { \
    const any& lhs = any::unthunkVariant(lhs_); \
    const any& rhs = any::unthunkVariant(rhs_); \
    switch (lhs.type) { \
      case any::Type::Integer:   assert(lhs.type == any::Type::Integer);   return lhs.i op rhs.i; \
      case any::Type::Double:    assert(lhs.type == any::Type::Double);    return lhs.d op rhs.d; \
      case any::Type::Character: assert(lhs.type == any::Type::Character); return lhs.c op rhs.c; \
      case any::Type::Boolean:   assert(lhs.type == any::Type::Boolean);   return lhs.b op rhs.b; \
      case any::Type::StringLiteral: \
        assert(lhs.type == any::Type::StringLiteral || lhs.type == any::Type::String); \
        assert(rhs.type == any::Type::StringLiteral || rhs.type == any::Type::String); \
        return strcmp(lhs.r, rhs.type == any::Type::StringLiteral ? rhs.r : rhs.s->c_str()) op 0; \
      case any::Type::String: \
        assert(lhs.type == any::Type::StringLiteral || lhs.type == any::Type::String); \
        assert(rhs.type == any::Type::StringLiteral || rhs.type == any::Type::String); \
        return strcmp(lhs.s->c_str(), rhs.type == any::Type::StringLiteral ? rhs.r : rhs.s->c_str()) op 0; \
      case any::Type::Pointer:   return lhs.p op rhs.p; \
      default: assert(false && "Unsupported type for operator " #op); \
    } \
    return false; \
  } \
  DEFINE_OPERATOR(op, long, bool) \
  DEFINE_OPERATOR(op, double, bool) \
  DEFINE_OPERATOR(op, char, bool) \
  DEFINE_OPERATOR(op, bool, bool) \

//-----------------------------------------------------------------------------
// Operator definitions
//-----------------------------------------------------------------------------

DEFINE_COMPARISON_OPERATOR(==)
DEFINE_COMPARISON_OPERATOR(!=)
DEFINE_COMPARISON_OPERATOR(<)
DEFINE_COMPARISON_OPERATOR(<=)
DEFINE_COMPARISON_OPERATOR(>)
DEFINE_COMPARISON_OPERATOR(>=)

DEFINE_CSTR_EQUALS_OPERATOR()
DEFINE_CSTR_COMPARISON_OPERATOR(!=)
DEFINE_CSTR_COMPARISON_OPERATOR(<)
DEFINE_CSTR_COMPARISON_OPERATOR(<=)
DEFINE_CSTR_COMPARISON_OPERATOR(>)
DEFINE_CSTR_COMPARISON_OPERATOR(>=)

auto operator+(const any& lhs_, const any& rhs_) -> any {
  const any& lhs = any::unthunkVariant(lhs_);
  const any& rhs = any::unthunkVariant(rhs_);
  switch (lhs.type) {
    case any::Type::Integer:       assert(lhs.type == any::Type::Integer);   return lhs.i + rhs.i;
    case any::Type::Double:        assert(lhs.type == any::Type::Double);    return lhs.d + rhs.d;
    case any::Type::Character:     assert(lhs.type == any::Type::Character); return any(char(lhs.c + rhs.c));
    case any::Type::StringLiteral:
      assert(lhs.type == any::Type::StringLiteral || lhs.type == any::Type::String);
      assert(rhs.type == any::Type::StringLiteral || rhs.type == any::Type::String);
      return rhs.type == any::Type::StringLiteral ? std::string(lhs.r) + rhs.r : lhs.r + *rhs.s;
    case any::Type::String:
      assert(lhs.type == any::Type::StringLiteral || lhs.type == any::Type::String);
      assert(rhs.type == any::Type::StringLiteral || rhs.type == any::Type::String);
      return rhs.type == any::Type::StringLiteral ? *lhs.s + rhs.r : *lhs.s + *rhs.s;
    default: assert(false && "Unsupported type for '+' operator");
  }
  return nullptr;
}

DEFINE_OPERATOR(+, long, long)
DEFINE_OPERATOR(+, double, double)
DEFINE_OPERATOR(+, char, char)

auto operator+(const any& lhs_, const char * rhs) -> std::string {
  const any& lhs = any::unthunkVariant(lhs_);
  assert(lhs.type == any::Type::StringLiteral || lhs.type == any::Type::String);
  return lhs.type == any::Type::StringLiteral ? std::string(lhs.r) + rhs : *lhs.s + rhs;
}

auto operator+(const char * lhs, const any& rhs_) -> std::string {
  const any& rhs = any::unthunkVariant(rhs_);
  assert(rhs.type == any::Type::StringLiteral || rhs.type == any::Type::String);
  return rhs.type == any::Type::StringLiteral ? lhs + std::string(rhs.r) : lhs + *rhs.s;
}

auto operator-(const any& lhs_, const any& rhs_) -> any {
  const any& lhs = any::unthunkVariant(lhs_);
  const any& rhs = any::unthunkVariant(rhs_);
  assert(lhs.type == rhs.type);
  switch (lhs.type) {
    case any::Type::Integer:   return lhs.i - rhs.i;
    case any::Type::Double:    return lhs.d - rhs.d;
    case any::Type::Character: return any(char(lhs.c - rhs.c));
    default: assert(false && "Unsupported type for '-' operator");
  }
  return nullptr;
}

DEFINE_OPERATOR(-, long, long)
DEFINE_OPERATOR(-, double, double)
DEFINE_OPERATOR(-, char, char)

auto operator*(const any& lhs_, const any& rhs_) -> any {
  const any& lhs = any::unthunkVariant(lhs_);
  const any& rhs = any::unthunkVariant(rhs_);
  assert(lhs.type == rhs.type);
  switch (lhs.type) {
    case any::Type::Integer:   return lhs.i * rhs.i;
    case any::Type::Double:    return lhs.d * rhs.d;
    case any::Type::Character: return any(char(lhs.c * rhs.c));
    default: assert(false && "Unsupported type for '*' operator");
  }
  return nullptr;
}

DEFINE_OPERATOR(*, long, long)
DEFINE_OPERATOR(*, double, double)
DEFINE_OPERATOR(*, char, char)

auto operator/(const any& lhs_, const any& rhs_) -> any {
  const any& lhs = any::unthunkVariant(lhs_);
  const any& rhs = any::unthunkVariant(rhs_);
  assert(lhs.type == rhs.type);
  switch (lhs.type) {
    case any::Type::Integer:   return lhs.i / rhs.i;
    case any::Type::Double:    return lhs.d / rhs.d;
    case any::Type::Character: return any(char(lhs.c / rhs.c));
    default: assert(false && "Unsupported type for '/' operator");
  }
  return nullptr;
}

DEFINE_OPERATOR(/, long, long)
DEFINE_OPERATOR(/, double, double)
DEFINE_OPERATOR(/, char, char)

auto operator%(const any& lhs_, const any& rhs_) -> any {
  const any& lhs = any::unthunkVariant(lhs_);
  const any& rhs = any::unthunkVariant(rhs_);
  assert(lhs.type == rhs.type);
  switch (lhs.type) {
    case any::Type::Integer:   return lhs.i % rhs.i;
    case any::Type::Character: return any(char(lhs.c % rhs.c));
    default: assert(false && "Unsupported type for '%' operator");
  }
  return nullptr;
}

DEFINE_OPERATOR(%, long, long)
DEFINE_OPERATOR(%, char, char)

// unary negate
auto operator-(const any& rhs_) -> any {
  const any& rhs = any::unthunkVariant(rhs_);
  switch (rhs.type) {
    case any::Type::Integer: return (- rhs.i);
    case any::Type::Double:  return (- rhs.d);
    default: assert(false && "Unsupported type for unary '-' operator");
  }
  return nullptr;
}

} // namespace PureScript
