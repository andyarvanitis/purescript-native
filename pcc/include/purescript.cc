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
#include "PureScript.hh"

namespace PureScript {

any::any(const any& val) : type(val.type) {
  switch (type) {
    case Type::Integer:         i = val.i;                         break;
    case Type::Double:          d = val.d;                         break;
    case Type::Character:       c = val.c;                         break;
    case Type::Boolean:         b = val.b;                         break;
    case Type::String:          new (&s) shared_string   (val.s);  break;
    case Type::Map:             new (&m) shared_map      (val.m);  break;
    case Type::Vector:          new (&v) shared_vector   (val.v);  break;
    case Type::Function:        new (&f) shared_fn       (val.f);  break;
    case Type::EffFunction:     new (&e) shared_eff_fn   (val.e);  break;
    case Type::Thunk:           new (&t) shared_thunk    (val.t);  break;
    case Type::Pointer:         new (&p) shared_void_ptr (val.p);  break;

    default: assert(not "supported type in copy ctor");
  }
}

void any::swap(any&& val) {
  type = val.type;
  switch (type) {
    case Type::Integer:         i = val.i;                                    break;
    case Type::Double:          d = val.d;                                    break;
    case Type::Character:       c = val.c;                                    break;
    case Type::Boolean:         b = val.b;                                    break;
    case Type::String:          new (&s) shared_string   (std::move(val.s));  break;
    case Type::Map:             new (&m) shared_map      (std::move(val.m));  break;
    case Type::Vector:          new (&v) shared_vector   (std::move(val.v));  break;
    case Type::Function:        new (&f) shared_fn       (std::move(val.f));  break;
    case Type::EffFunction:     new (&e) shared_eff_fn   (std::move(val.e));  break;
    case Type::Thunk:           new (&t) shared_thunk    (std::move(val.t));  break;
    case Type::Pointer:         new (&p) shared_void_ptr (std::move(val.p));  break;

    default: assert(not "supported type in move ctor");
  }
}

any::~any() {
  // std::cout << "destroy" << std::endl;
  switch (type) {
    case Type::Integer:         ;                     break;
    case Type::Double:          ;                     break;
    case Type::Character:       ;                     break;
    case Type::Boolean:         ;                     break;
    case Type::String:          s.~shared_string();   break;
    case Type::Map:             m.~shared_map();      break;
    case Type::Vector:          v.~shared_vector();   break;
    case Type::Function:        f.~shared_fn();       break;
    case Type::EffFunction:     e.~shared_eff_fn();   break;
    case Type::Thunk:           t.~shared_thunk();    break;
    case Type::Pointer:         p.~shared_void_ptr(); break;

    default: assert(not "supported type in destructor");
  }
};

#define RETURN_VALUE(TYPE, VAL, FN) \
  if (type == TYPE) { \
    return FN(VAL); \
  } else { \
    assert(type == Type::Thunk); \
    const any& value = (*t)(unthunk); \
    assert(value.type == TYPE); \
    return FN(value.VAL); \
  }

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
  auto any::cast() const -> typename std::enable_if<std::is_same<T, string>::value, const T&>::type {
    RETURN_VALUE(Type::String, s, *)
  }
  template auto any::cast<string>() const -> const string&;

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


  auto any::operator()(const any arg) const -> any {
    if (type == Type::Function) {
      return (*f)(arg);
    } else {
      assert(type == Type::Thunk);
      const any& value = (*t)(unthunk);
      assert(value.type == Type::Function);
      return (*value.f)(arg);
    }
  }

  auto any::operator()(const as_thunk) const -> const any& {
    assert(type == Type::Thunk);
    return (*t)(unthunk);
  }

  auto any::call(const any& a) -> any {
    assert(a.type == Type::EffFunction || a.type == Type::Function);
    if (a.type == Type::EffFunction) {
      return (*a.e)();
    } else {
      return (*a.f)(false);
    }
  }

  auto any::operator()() const -> any {
    if (type == Type::EffFunction || type == Type::Function) {
      return call(*this);
    } else {
      assert(type == Type::Thunk);
      const any& value = (*t)(unthunk);
      return call(value);
    }
  }

  any::operator long() const {
    RETURN_VALUE(Type::Integer, i,)
  }

  any::operator double() const {
    RETURN_VALUE(Type::Double, d,)
  }

  any::operator bool() const {
    RETURN_VALUE(Type::Boolean, b,)
  }

  any::operator const string&() const {
    RETURN_VALUE(Type::String, s, *)
  }

  any::operator const map&() const {
    RETURN_VALUE(Type::Map, m, *)
  }

  any::operator const vector&() const {
    RETURN_VALUE(Type::Vector, v, *)
  }

  auto any::extractPointer() const -> const void* {
    RETURN_VALUE(Type::Pointer, p.get(),)
  }

  auto any::extract_value(const any& a) -> const any& {
    return a.type != Type::Thunk ? a : (*a.t)(unthunk);
  }

  auto any::operator[](const map_key_t rhs) const -> const any& {
    RETURN_VALUE(Type::Map, m->at(rhs),)
  }

  auto any::operator[](const vector::size_type rhs) const -> const any& {
    RETURN_VALUE(Type::Vector, v->at(rhs),)
  }

  auto any::operator[](const any& rhs) const -> const any& {
    RETURN_VALUE(Type::Vector, v->at(rhs.cast<long>()),)
  }

#define DEFINE_OPERATOR_LHS(K, T, OP, V, R, P) \
auto operator OP(const T lhs, const any& rhs_) -> R { \
  auto& rhs = any::extract_value(rhs_); \
  assert(rhs.type == any::Type::K); \
  return lhs OP P(rhs.V); \
}

#define DEFINE_OPERATOR_RHS(K, T, OP, V, R, P) \
auto any::operator OP(const T rhs) const -> R { \
  auto& lhs = any::extract_value(*this); \
  assert(lhs.type == Type::K); \
  return P(lhs.V) OP rhs; \
}

auto any::operator==(const any& rhs_) const -> bool {
  auto& lhs = extract_value(*this);
  auto& rhs = extract_value(rhs_);
  assert(lhs.type == rhs.type);
  switch (lhs.type) {
    case Type::Integer:   return lhs.i == rhs.i;
    case Type::Double:    return lhs.d == rhs.d;
    case Type::Character: return lhs.c == rhs.c;
    case Type::Boolean:   return lhs.b == rhs.b;
    case Type::String:    return (*lhs.s) == (*rhs.s);
    case Type::Pointer:   return lhs.p == rhs.p;
    default: assert(not "supported type for '==' operator");
  }
  return false;
}

DEFINE_OPERATOR_RHS(Integer,   long,        ==, i, bool,)
DEFINE_OPERATOR_RHS(Double,    double,      ==, d, bool,)
DEFINE_OPERATOR_RHS(Character, char,        ==, c, bool,)
DEFINE_OPERATOR_RHS(Boolean,   bool,        ==, b, bool,)
DEFINE_OPERATOR_RHS(String,    string&,     ==, s, bool, *)
DEFINE_OPERATOR_RHS(String,    char* const, ==, s, bool, *)

auto any::operator!=(const any& rhs_) const -> bool {
  auto& lhs = extract_value(*this);
  auto& rhs = extract_value(rhs_);
  assert(lhs.type == rhs.type);
  switch (lhs.type) {
    case Type::Integer:   return lhs.i != rhs.i;
    case Type::Double:    return lhs.d != rhs.d;
    case Type::Character: return lhs.c != rhs.c;
    case Type::Boolean:   return lhs.b != rhs.b;
    case Type::String:    return (*lhs.s) != (*rhs.s);
    case Type::Pointer:   return lhs.p != rhs.p;
    default: assert(not "supported type for '!=' operator");
  }
  return false;
}

DEFINE_OPERATOR_RHS(Integer,   long,        !=, i, bool,)
DEFINE_OPERATOR_RHS(Double,    double,      !=, d, bool,)
DEFINE_OPERATOR_RHS(Character, char,        !=, c, bool,)
DEFINE_OPERATOR_RHS(Boolean,   bool,        !=, b, bool,)
DEFINE_OPERATOR_RHS(String,    string&,     !=, s, bool, *)
DEFINE_OPERATOR_RHS(String,    char* const, !=, s, bool, *)

auto any::operator<(const any& rhs_) const -> bool {
  auto& lhs = extract_value(*this);
  auto& rhs = extract_value(rhs_);
  assert(lhs.type == rhs.type);
  switch (lhs.type) {
    case Type::Integer:   return lhs.i < rhs.i;
    case Type::Double:    return lhs.d < rhs.d;
    case Type::Character: return lhs.c < rhs.c;
    case Type::Boolean:   return lhs.b < rhs.b;
    case Type::String:    return (*lhs.s) < (*rhs.s);
    default: assert(not "supported type for '<' operator");
  }
  return false;
}

DEFINE_OPERATOR_RHS(Integer,   long,        <, i, bool,)
DEFINE_OPERATOR_RHS(Double,    double,      <, d, bool,)
DEFINE_OPERATOR_RHS(Character, char,        <, c, bool,)
DEFINE_OPERATOR_RHS(Boolean,   bool,        <, b, bool,)
DEFINE_OPERATOR_RHS(String,    string&,     <, s, bool, *)
DEFINE_OPERATOR_RHS(String,    char* const, <, s, bool, *)

auto any::operator<=(const any& rhs_) const -> bool {
  auto& lhs = extract_value(*this);
  auto& rhs = extract_value(rhs_);
  assert(lhs.type == rhs.type);
  switch (lhs.type) {
    case Type::Integer:   return lhs.i <= rhs.i;
    case Type::Double:    return lhs.d <= rhs.d;
    case Type::Character: return lhs.c <= rhs.c;
    case Type::Boolean:   return lhs.b <= rhs.b;
    case Type::String:    return (*lhs.s) <= (*rhs.s);
    default: assert(not "supported type for '<' operator");
  }
  return false;
}

DEFINE_OPERATOR_RHS(Integer,   long,        <=, i, bool,)
DEFINE_OPERATOR_RHS(Double,    double,      <=, d, bool,)
DEFINE_OPERATOR_RHS(Character, char,        <=, c, bool,)
DEFINE_OPERATOR_RHS(Boolean,   bool,        <=, b, bool,)
DEFINE_OPERATOR_RHS(String,    string&,     <=, s, bool, *)
DEFINE_OPERATOR_RHS(String,    char* const, <=, s, bool, *)

auto any::operator>(const any& rhs_) const -> bool {
  auto& lhs = extract_value(*this);
  auto& rhs = extract_value(rhs_);
  assert(lhs.type == rhs.type);
  switch (lhs.type) {
    case Type::Integer:   return lhs.i > rhs.i;
    case Type::Double:    return lhs.d > rhs.d;
    case Type::Character: return lhs.c > rhs.c;
    case Type::Boolean:   return lhs.b > rhs.b;
    case Type::String:    return (*lhs.s) > (*rhs.s);
    default: assert(not "supported type for '>' operator");
  }
  return false;
}

DEFINE_OPERATOR_RHS(Integer,   long,        >, i, bool,)
DEFINE_OPERATOR_RHS(Double,    double,      >, d, bool,)
DEFINE_OPERATOR_RHS(Character, char,        >, c, bool,)
DEFINE_OPERATOR_RHS(Boolean,   bool,        >, b, bool,)
DEFINE_OPERATOR_RHS(String,    string&,     >, s, bool, *)
DEFINE_OPERATOR_RHS(String,    char* const, >, s, bool, *)

auto any::operator>=(const any& rhs_) const -> bool {
  auto& lhs = extract_value(*this);
  auto& rhs = extract_value(rhs_);
  assert(lhs.type == rhs.type);
  switch (lhs.type) {
    case Type::Integer:   return lhs.i >= rhs.i;
    case Type::Double:    return lhs.d >= rhs.d;
    case Type::Character: return lhs.c >= rhs.c;
    case Type::Boolean:   return lhs.b >= rhs.b;
    case Type::String:    return (*lhs.s) >= (*rhs.s);
    default: assert(not "supported type for '>' operator");
  }
  return false;
}

DEFINE_OPERATOR_RHS(Integer,   long,        >=, i, bool,)
DEFINE_OPERATOR_RHS(Double,    double,      >=, d, bool,)
DEFINE_OPERATOR_RHS(Character, char,        >=, c, bool,)
DEFINE_OPERATOR_RHS(Boolean,   bool,        >=, b, bool,)
DEFINE_OPERATOR_RHS(String,    string&,     >=, s, bool, *)
DEFINE_OPERATOR_RHS(String,    char* const, >=, s, bool, *)

auto any::operator+(const any& rhs_) const -> any {
  auto& lhs = extract_value(*this);
  auto& rhs = extract_value(rhs_);
  assert(lhs.type == rhs.type);
  switch (lhs.type) {
    case Type::Integer:   return lhs.i + rhs.i;
    case Type::Double:    return lhs.d + rhs.d;
    case Type::Character: return lhs.c + rhs.c;
    case Type::String:    return (*lhs.s) + (*rhs.s);
    default: assert(not "supported type for '+' operator");
  }
  return nullptr;
}

DEFINE_OPERATOR_RHS(Integer,   long,        +, i, long,)
DEFINE_OPERATOR_RHS(Double,    double,      +, d, double,)
DEFINE_OPERATOR_RHS(Character, char,        +, c, char,)
DEFINE_OPERATOR_RHS(String,    string&,     +, s, string, *)
DEFINE_OPERATOR_RHS(String,    char* const, +, s, string, *)

auto any::operator-(const any& rhs_) const -> any {
  auto& lhs = extract_value(*this);
  auto& rhs = extract_value(rhs_);
  assert(lhs.type == rhs.type);
  switch (lhs.type) {
    case Type::Integer:   return lhs.i - rhs.i;
    case Type::Double:    return lhs.d - rhs.d;
    case Type::Character: return lhs.c - rhs.c;
    default: assert(not "supported type for '-' binary operator");
  }
  return nullptr;
}

DEFINE_OPERATOR_RHS(Integer,   long,   -, i, long,)
DEFINE_OPERATOR_RHS(Double,    double, -, d, double,)
DEFINE_OPERATOR_RHS(Character, char,   -, c, char,)

auto any::operator*(const any& rhs_) const -> any {
  auto& lhs = extract_value(*this);
  auto& rhs = extract_value(rhs_);
  assert(lhs.type == rhs.type);
  switch (lhs.type) {
    case Type::Integer: return lhs.i * rhs.i;
    case Type::Double:  return lhs.d * rhs.d;
    default: assert(not "supported type for '*' operator");
  }
  return nullptr;
}

DEFINE_OPERATOR_RHS(Integer,   long,   *, i, long,)
DEFINE_OPERATOR_RHS(Double,    double, *, d, double,)

auto any::operator/(const any& rhs_) const -> any {
  auto& lhs = extract_value(*this);
  auto& rhs = extract_value(rhs_);
  assert(lhs.type == rhs.type);
  switch (lhs.type) {
    case Type::Integer: return lhs.i / rhs.i;
    case Type::Double:  return lhs.d / rhs.d;
    default: assert(not "supported type for '/' operator");
  }
  return nullptr;
}

DEFINE_OPERATOR_RHS(Integer,   long,   /, i, long,)
DEFINE_OPERATOR_RHS(Double,    double, /, d, double,)

auto any::operator%(const any& rhs_) const -> any {
  auto& lhs = extract_value(*this);
  auto& rhs = extract_value(rhs_);
  assert(lhs.type == rhs.type);
  switch (lhs.type) {
    case Type::Integer: return lhs.i % rhs.i;
    default: assert(not "supported type for '%' operator");
  }
  return nullptr;
}

DEFINE_OPERATOR_RHS(Integer,   long, %, i, long,)

auto any::operator-() const -> any {
  auto& lhs = any::extract_value(*this);
  switch (lhs.type) {
    case Type::Integer: return (- lhs.i);
    case Type::Double:  return (- lhs.d);
    default: assert(not "supported type for unary '-' operator");
  }
  return nullptr;
}

DEFINE_OPERATOR_LHS(Integer, long, ==, i, bool,)
DEFINE_OPERATOR_LHS(Integer, long, !=, i, bool,)
DEFINE_OPERATOR_LHS(Integer, long, <,  i, bool,)
DEFINE_OPERATOR_LHS(Integer, long, <=, i, bool,)
DEFINE_OPERATOR_LHS(Integer, long, >,  i, bool,)
DEFINE_OPERATOR_LHS(Integer, long, >=, i, bool,)

DEFINE_OPERATOR_LHS(Double, double, ==, d, bool,)
DEFINE_OPERATOR_LHS(Double, double, !=, d, bool,)
DEFINE_OPERATOR_LHS(Double, double, <,  d, bool,)
DEFINE_OPERATOR_LHS(Double, double, <=, d, bool,)
DEFINE_OPERATOR_LHS(Double, double, >,  d, bool,)
DEFINE_OPERATOR_LHS(Double, double, >=, d, bool,)

DEFINE_OPERATOR_LHS(Character, char, ==, c, bool,)
DEFINE_OPERATOR_LHS(Character, char, !=, c, bool,)
DEFINE_OPERATOR_LHS(Character, char, <,  c, bool,)
DEFINE_OPERATOR_LHS(Character, char, <=, c, bool,)
DEFINE_OPERATOR_LHS(Character, char, >,  c, bool,)
DEFINE_OPERATOR_LHS(Character, char, >=, c, bool,)

DEFINE_OPERATOR_LHS(Boolean, bool, ==, b, bool,)
DEFINE_OPERATOR_LHS(Boolean, bool, !=, b, bool,)
DEFINE_OPERATOR_LHS(Boolean, bool, <,  b, bool,)
DEFINE_OPERATOR_LHS(Boolean, bool, <=, b, bool,)
DEFINE_OPERATOR_LHS(Boolean, bool, >,  b, bool,)
DEFINE_OPERATOR_LHS(Boolean, bool, >=, b, bool,)

DEFINE_OPERATOR_LHS(String, string&, ==, s, bool, *)
DEFINE_OPERATOR_LHS(String, string&, !=, s, bool, *)
DEFINE_OPERATOR_LHS(String, string&, <,  s, bool, *)
DEFINE_OPERATOR_LHS(String, string&, <=, s, bool, *)
DEFINE_OPERATOR_LHS(String, string&, >,  s, bool, *)
DEFINE_OPERATOR_LHS(String, string&, >=, s, bool, *)

DEFINE_OPERATOR_LHS(String, char* const, ==, s, bool, *)
DEFINE_OPERATOR_LHS(String, char* const, !=, s, bool, *)
DEFINE_OPERATOR_LHS(String, char* const, <,  s, bool, *)
DEFINE_OPERATOR_LHS(String, char* const, <=, s, bool, *)
DEFINE_OPERATOR_LHS(String, char* const, >,  s, bool, *)
DEFINE_OPERATOR_LHS(String, char* const, >=, s, bool, *)

DEFINE_OPERATOR_LHS(Integer, long, +, i, long,)
DEFINE_OPERATOR_LHS(Integer, long, -, i, long,)
DEFINE_OPERATOR_LHS(Integer, long, *, i, long,)
DEFINE_OPERATOR_LHS(Integer, long, /, i, long,)
DEFINE_OPERATOR_LHS(Integer, long, %, i, long,)

DEFINE_OPERATOR_LHS(Double, double, +, d, double,)
DEFINE_OPERATOR_LHS(Double, double, -, d, double,)
DEFINE_OPERATOR_LHS(Double, double, *, d, double,)
DEFINE_OPERATOR_LHS(Double, double, /, d, double,)

DEFINE_OPERATOR_LHS(Character, char, +, i, char,)
DEFINE_OPERATOR_LHS(Character, char, -, i, char,)

DEFINE_OPERATOR_LHS(String, string&, +, s, string, *)
DEFINE_OPERATOR_LHS(String, char* const, +, s, string, *)


} // namespace PureScript
