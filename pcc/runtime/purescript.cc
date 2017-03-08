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
    return (*static_cast<const Closure*>(POINTER_FROM_MEMBER(variant.p)))(arg);
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
    return (*static_cast<const EffClosure*>(POINTER_FROM_MEMBER(variant.p)))();
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

any::operator char32_t() const {
  RETURN_VALUE(Tag::Character, c,)
}

any::operator char() const {
  const auto val = static_cast<decltype(c)>(*this);
  assert(val <= std::numeric_limits<char>::max());
  return static_cast<char>(val);
}

any::operator size_t() const {
  const auto val = static_cast<decltype(i)>(*this);
  assert(val >= 0);
  return static_cast<size_t>(val);
}

any::operator const string&() const {
  return *static_cast<const string*>(extractPointer(IF_DEBUG(Tag::String)));
}

any::operator const char *() const {
  return static_cast<const string*>(extractPointer(IF_DEBUG(Tag::String)))->c_str();
}

any::operator const array&() const {
  return *static_cast<const array*>(extractPointer(IF_DEBUG(Tag::Array)));
}

any::operator const record&() const {
  return *static_cast<const record*>(extractPointer(IF_DEBUG(Tag::Record)));
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

auto any::operator[](const int rhs) const -> const any& {
  assert(rhs >= 0);
  const auto& xs = static_cast<const array&>(*this);
  return xs[rhs];
}

auto any::size() const -> size_t {
  return static_cast<const array&>(*this).size();
}

auto any::empty() const -> bool {
  return static_cast<const array&>(*this).empty();
}

auto any::contains(const Private::Symbol key) const -> bool {
  // TODO: assumes at least one element -- safe assumption?
  const auto& m =
    *static_cast<const any::dict<unknown_size>*>(this->extractPointer(IF_DEBUG(any::Tag::Dictionary)));
  std::remove_reference<decltype(m)>::type::size_type i = 0;
  do {
    if (m[i].first == key) {
      return true;
    }
  } while (m[++i].first != nullptr);
  return false;
}

auto any::cstr_cmp::operator ()(const char * a, const char * b) const -> bool {
  return a == b ? false : std::strcmp(a, b) < 0;
}

auto any::at(const char * key) const -> const any& {
  return static_cast<const record&>(*this).at(key);
}

//-----------------------------------------------------------------------------
// Operator helper macros
//-----------------------------------------------------------------------------

#define DEFINE_COMPARISON_OPERATOR(op) \
  auto operator op (const any& lhs_, const any& rhs) -> bool { \
    const any& lhs = any::unthunkVariant(lhs_); \
    switch (lhs.tag) { \
      case any::Tag::Integer:    return lhs.i op static_cast<decltype(any::i)>(rhs); \
      case any::Tag::Double:     return lhs.d op static_cast<decltype(any::d)>(rhs); \
      case any::Tag::Character:  return lhs.c op static_cast<decltype(any::c)>(rhs); \
      case any::Tag::Boolean:    return lhs.b op static_cast<decltype(any::b)>(rhs); \
      case any::Tag::String:     return *static_cast<const string*>(POINTER_FROM_MEMBER(lhs.p)) \
                                      op static_cast<const string&>(rhs); \
      case any::Tag::RawPointer: return lhs.v op static_cast<decltype(any::v)>(rhs); \
      case any::Tag::Pointer:    return lhs.p op any::unthunkVariant(rhs).p; \
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

auto operator+(const any& lhs_, const any& rhs) -> any {
  const any& lhs = any::unthunkVariant(lhs_);
  switch (lhs.tag) {
    case any::Tag::Integer:   return lhs.i + static_cast<decltype(any::i)>(rhs);
    case any::Tag::Double:    return lhs.d + static_cast<decltype(any::d)>(rhs);
    case any::Tag::Character: return decltype(any::c){lhs.c + static_cast<decltype(any::c)>(rhs)};
    case any::Tag::String:    return *static_cast<const string*>(POINTER_FROM_MEMBER(lhs.p))
                                    + static_cast<const string&>(rhs);
    default: assert(false && "Unsupported tag for '+' operator");
  }
  return nullptr;
}

auto operator-(const any& lhs_, const any& rhs) -> any {
  const any& lhs = any::unthunkVariant(lhs_);
  switch (lhs.tag) {
    case any::Tag::Integer:   return lhs.i - static_cast<decltype(any::i)>(rhs);
    case any::Tag::Double:    return lhs.d - static_cast<decltype(any::d)>(rhs);
    case any::Tag::Character: return decltype(any::c){lhs.c - static_cast<decltype(any::c)>(rhs)};
    default: assert(false && "Unsupported tag for '-' operator");
  }
  return nullptr;
}

auto operator*(const any& lhs_, const any& rhs) -> any {
  const any& lhs = any::unthunkVariant(lhs_);
  switch (lhs.tag) {
    case any::Tag::Integer:   return lhs.i * static_cast<decltype(any::i)>(rhs);
    case any::Tag::Double:    return lhs.d * static_cast<decltype(any::d)>(rhs);
    case any::Tag::Character: return decltype(lhs.c){lhs.c - static_cast<decltype(any::c)>(rhs)};
    default: assert(false && "Unsupported tag for '*' operator");
  }
  return nullptr;
}

auto operator/(const any& lhs_, const any& rhs) -> any {
  const any& lhs = any::unthunkVariant(lhs_);
  switch (lhs.tag) {
    case any::Tag::Integer:   return lhs.i / static_cast<decltype(any::i)>(rhs);
    case any::Tag::Double:    return lhs.d / static_cast<decltype(any::d)>(rhs);
    case any::Tag::Character: return decltype(any::c){lhs.c / static_cast<decltype(any::c)>(rhs)};
    default: assert(false && "Unsupported tag for '/' operator");
  }
  return nullptr;
}

auto operator%(const any& lhs_, const any& rhs) -> any {
  const any& lhs = any::unthunkVariant(lhs_);
  switch (lhs.tag) {
    case any::Tag::Integer:   return lhs.i % static_cast<decltype(any::i)>(rhs);
    case any::Tag::Character: return decltype(any::c){lhs.c % static_cast<decltype(any::c)>(rhs)};
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
