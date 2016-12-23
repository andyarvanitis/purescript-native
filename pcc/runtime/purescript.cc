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

any::Closure::~Closure() {}
any::EffClosure::~EffClosure() {}

#define RETURN_VALUE(TAG, ACCESSOR, INDIRECTION) \
  const any& variant = unthunkVariant(*this); \
  assert(variant.tag == TAG); \
  return INDIRECTION(variant.ACCESSOR); \

auto any::operator()(const any& arg) const -> any {
  const any& variant = unthunkVariant(*this);
  if (variant.tag == Tag::Closure) {
    return (*variant.l)(arg);
  }
  assert(variant.tag == Tag::Function);
  return (*variant.f)(arg);
}

auto any::operator()(const as_thunk) const -> const any& {
  assert(tag == Tag::Thunk);
  return (*t)(unthunk);
}

auto any::operator()() const -> any {
  const any& variant = unthunkVariant(*this);
  if (variant.tag == Tag::EffClosure) {
    return (*variant.k)();
  }
  assert(variant.tag == Tag::EffFunction);
  return (*variant.e)();
}

any::operator int() const {
  RETURN_VALUE(Tag::Integer, i,)
}

any::operator double() const {
  RETURN_VALUE(Tag::Double, d,)
}

any::operator bool() const {
  RETURN_VALUE(Tag::Boolean, b,)
}

any::operator char() const {
  RETURN_VALUE(Tag::Character, c,)
}

any::operator size_t() const {
  const auto sz = cast<int>(*this);
  assert(sz >= 0);
  return static_cast<size_t>(sz);
}

any::operator cstring() const {
  const any& variant = unthunkVariant(*this);
  if (variant.tag == Tag::StringLiteral) {
    return variant.r;
  }
  assert(variant.tag == Tag::String);
  return variant.s->c_str();
}

any::operator const array&() const {
  RETURN_VALUE(Tag::Array, a, *)
}

auto any::extractPointer(IF_DEBUG(const any::Tag t)) const -> void* {
  RETURN_VALUE(t, POINTER_FROM_MEMBER(p),)
}

auto any::unthunkVariant(const any& a) -> const any& {
  const any * variant = &a;
  while (variant->tag == Tag::Thunk) {
    variant = &((*variant->t)(unthunk));
  }
  return *variant;
}

auto any::operator[](const size_t rhs) const -> const any& {
  const auto& xs = cast<array>(*this);
  return xs[rhs];
}

auto any::size() const -> size_t {
  return cast<array>(*this).size();
}

auto any::empty() const -> bool {
  return cast<array>(*this).empty();
}

auto any::contains(const Private::Symbol key) const -> bool {
  // TODO: assumes at least one element -- safe assumption?
  const auto& m = cast<map<unknown_size>>(*this);
  std::remove_reference<decltype(m)>::type::size_type i = 0;
  do {
    if (m[i].first == key) {
      return true;
    }
  } while (m[++i].first != nullptr);
  return false;
}

//-----------------------------------------------------------------------------
// Operator helper macros
//-----------------------------------------------------------------------------

#define DEFINE_CSTR_EQUALS_OPERATOR() \
  auto operator==(const any& lhs_, const char * rhs) -> bool { \
    const any& lhs = any::unthunkVariant(lhs_); \
    assert(lhs.tag == any::Tag::StringLiteral || lhs.tag == any::Tag::String); \
    if (lhs.tag == any::Tag::StringLiteral) { \
      return (lhs.r == rhs) || (strcmp(lhs.r, rhs) == 0); \
    } \
    return strcmp(lhs.s->c_str(), rhs) == 0; \
  } \
  auto operator==(const char * lhs, const any& rhs_) -> bool { \
    const any& rhs = any::unthunkVariant(rhs_); \
    assert(rhs.tag == any::Tag::StringLiteral || rhs.tag == any::Tag::String); \
    if (rhs.tag == any::Tag::StringLiteral) { \
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
    switch (lhs.tag) { \
      case any::Tag::Integer:   assert(lhs.tag == any::Tag::Integer);   return lhs.i op rhs.i; \
      case any::Tag::Double:    assert(lhs.tag == any::Tag::Double);    return lhs.d op rhs.d; \
      case any::Tag::Character: assert(lhs.tag == any::Tag::Character); return lhs.c op rhs.c; \
      case any::Tag::Boolean:   assert(lhs.tag == any::Tag::Boolean);   return lhs.b op rhs.b; \
      case any::Tag::StringLiteral: \
        assert(lhs.tag == any::Tag::StringLiteral || lhs.tag == any::Tag::String); \
        assert(rhs.tag == any::Tag::StringLiteral || rhs.tag == any::Tag::String); \
        return strcmp(lhs.r, rhs.tag == any::Tag::StringLiteral ? rhs.r : rhs.s->c_str()) op 0; \
      case any::Tag::String: \
        assert(lhs.tag == any::Tag::StringLiteral || lhs.tag == any::Tag::String); \
        assert(rhs.tag == any::Tag::StringLiteral || rhs.tag == any::Tag::String); \
        return strcmp(lhs.s->c_str(), rhs.tag == any::Tag::StringLiteral ? rhs.r : rhs.s->c_str()) op 0; \
      case any::Tag::Pointer:   return lhs.p op rhs.p; \
      default: assert(false && "Unsupported tag for operator " #op); \
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
  switch (lhs.tag) {
    case any::Tag::Integer:       assert(lhs.tag == any::Tag::Integer);   return lhs.i + rhs.i;
    case any::Tag::Double:        assert(lhs.tag == any::Tag::Double);    return lhs.d + rhs.d;
    case any::Tag::Character:     assert(lhs.tag == any::Tag::Character); return any(char(lhs.c + rhs.c));
    case any::Tag::StringLiteral:
      assert(lhs.tag == any::Tag::StringLiteral || lhs.tag == any::Tag::String);
      assert(rhs.tag == any::Tag::StringLiteral || rhs.tag == any::Tag::String);
      return rhs.tag == any::Tag::StringLiteral ? std::string(lhs.r) + rhs.r : lhs.r + *rhs.s;
    case any::Tag::String:
      assert(lhs.tag == any::Tag::StringLiteral || lhs.tag == any::Tag::String);
      assert(rhs.tag == any::Tag::StringLiteral || rhs.tag == any::Tag::String);
      return rhs.tag == any::Tag::StringLiteral ? *lhs.s + rhs.r : *lhs.s + *rhs.s;
    default: assert(false && "Unsupported tag for '+' operator");
  }
  return nullptr;
}

auto operator+(const any& lhs_, const char * rhs) -> std::string {
  const any& lhs = any::unthunkVariant(lhs_);
  assert(lhs.tag == any::Tag::StringLiteral || lhs.tag == any::Tag::String);
  return lhs.tag == any::Tag::StringLiteral ? std::string(lhs.r) + rhs : *lhs.s + rhs;
}

auto operator+(const char * lhs, const any& rhs_) -> std::string {
  const any& rhs = any::unthunkVariant(rhs_);
  assert(rhs.tag == any::Tag::StringLiteral || rhs.tag == any::Tag::String);
  return rhs.tag == any::Tag::StringLiteral ? lhs + std::string(rhs.r) : lhs + *rhs.s;
}

auto operator-(const any& lhs_, const any& rhs_) -> any {
  const any& lhs = any::unthunkVariant(lhs_);
  const any& rhs = any::unthunkVariant(rhs_);
  assert(lhs.tag == rhs.tag);
  switch (lhs.tag) {
    case any::Tag::Integer:   return lhs.i - rhs.i;
    case any::Tag::Double:    return lhs.d - rhs.d;
    case any::Tag::Character: return any(char(lhs.c - rhs.c));
    default: assert(false && "Unsupported tag for '-' operator");
  }
  return nullptr;
}

auto operator*(const any& lhs_, const any& rhs_) -> any {
  const any& lhs = any::unthunkVariant(lhs_);
  const any& rhs = any::unthunkVariant(rhs_);
  assert(lhs.tag == rhs.tag);
  switch (lhs.tag) {
    case any::Tag::Integer:   return lhs.i * rhs.i;
    case any::Tag::Double:    return lhs.d * rhs.d;
    case any::Tag::Character: return any(char(lhs.c * rhs.c));
    default: assert(false && "Unsupported tag for '*' operator");
  }
  return nullptr;
}

auto operator/(const any& lhs_, const any& rhs_) -> any {
  const any& lhs = any::unthunkVariant(lhs_);
  const any& rhs = any::unthunkVariant(rhs_);
  assert(lhs.tag == rhs.tag);
  switch (lhs.tag) {
    case any::Tag::Integer:   return lhs.i / rhs.i;
    case any::Tag::Double:    return lhs.d / rhs.d;
    case any::Tag::Character: return any(char(lhs.c / rhs.c));
    default: assert(false && "Unsupported tag for '/' operator");
  }
  return nullptr;
}

auto operator%(const any& lhs_, const any& rhs_) -> any {
  const any& lhs = any::unthunkVariant(lhs_);
  const any& rhs = any::unthunkVariant(rhs_);
  assert(lhs.tag == rhs.tag);
  switch (lhs.tag) {
    case any::Tag::Integer:   return lhs.i % rhs.i;
    case any::Tag::Character: return any(char(lhs.c % rhs.c));
    default: assert(false && "Unsupported tag for '%' operator");
  }
  return nullptr;
}

// unary negate
auto operator-(const any& rhs_) -> any {
  const any& rhs = any::unthunkVariant(rhs_);
  switch (rhs.tag) {
    case any::Tag::Integer: return (- rhs.i);
    case any::Tag::Double:  return (- rhs.d);
    default: assert(false && "Unsupported tag for unary '-' operator");
  }
  return nullptr;
}

} // namespace PureScript
