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

#define RETURN_VALUE(TYPE, ACCESSOR, INDIRECTION) \
  const any& variant = unthunkVariant(*this); \
  assert(variant.type == TYPE); \
  return INDIRECTION(variant.ACCESSOR); \

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
  if (variant.type == Type::EffClosure) {
    return (*variant.k)();
  }
  assert(variant.type == Type::EffFunction);
  return (*variant.e)();
}

any::operator int() const {
  RETURN_VALUE(Type::Integer, i,)
}

any::operator double() const {
  RETURN_VALUE(Type::Double, d,)
}

any::operator bool() const {
  RETURN_VALUE(Type::Boolean, b,)
}

any::operator char() const {
  RETURN_VALUE(Type::Character, c,)
}

any::operator cstring() const {
  const any& variant = unthunkVariant(*this);
  if (variant.type == Type::StringLiteral) {
    return variant.r;
  }
  assert(variant.type == Type::String);
  return variant.s->c_str();
}

any::operator const map&() const {
  RETURN_VALUE(Type::Map, m, *)
}

any::operator const data&() const {
  RETURN_VALUE(Type::Data, v, *)
}

any::operator const array&() const {
  RETURN_VALUE(Type::Array, a, *)
}

auto any::extractPointer() const -> void* {
  RETURN_VALUE(Type::Pointer, POINTER_FROM_MEMBER(p),)
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
  const auto begin = variant.m->cbegin();
  const auto end = variant.m->cend();
  for (auto it = begin; it != end; ++it) {
    if (it->first == rhs) {
      return it->second;
    }
  }
  for (auto it = begin; it != end; ++it) {
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
  return (*variant.a)[cast<int>(rhs)];
}

auto any::contains(const char key[]) const -> bool {
  const any& variant = unthunkVariant(*this);
  assert(variant.type == Type::Map);
  const auto begin = variant.m->cbegin();
  const auto end = variant.m->cend();
  for (auto it = begin; it != end; ++it) {
    if (it->first == key) {
      return true;
    }
  }
  for (auto it = begin; it != end; ++it) {
    if (strcmp(it->first, key) == 0) {
      return true;
    }
  }
  return false;
}

//-----------------------------------------------------------------------------
// Operator helper macros
//-----------------------------------------------------------------------------

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
    return strcmp(cast<cstring>(lhs), rhs) op 0; \
  } \
  auto operator op (const char * lhs, const any& rhs) -> bool { \
    return strcmp(lhs, cast<cstring>(rhs)) op 0; \
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
  }

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
