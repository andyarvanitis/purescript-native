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

#define copy_data(src) \
  type = src.type; \
  switch (type) { \
    case Type::Integer:         i = src.i;                         break; \
    case Type::Double:          d = src.d;                         break; \
    case Type::Character:       c = src.c;                         break; \
    case Type::Boolean:         b = src.b;                         break; \
    case Type::String:          new (&s) shared<string>  (src.s);  break; \
    case Type::Map:             new (&m) shared<map>     (src.m);  break; \
    case Type::Vector:          new (&v) shared<vector>  (src.v);  break; \
    case Type::Function:        new (&f) shared<fn>      (src.f);  break; \
    case Type::EffFunction:     new (&e) shared<eff_fn>  (src.e);  break; \
    case Type::Thunk:           new (&t) shared<thunk>   (src.t);  break; \
    case Type::Pointer:         new (&p) shared<void>    (src.p);  break; \
    \
    default: assert(false && "Unsupported type in copy"); \
  }

#define move_data(src) \
  type = src.type; \
  switch (type) { \
    case Type::Integer:         i = src.i;                                    break; \
    case Type::Double:          d = src.d;                                    break; \
    case Type::Character:       c = src.c;                                    break; \
    case Type::Boolean:         b = src.b;                                    break; \
    case Type::String:          new (&s) shared<string>  (std::move(src.s));  break; \
    case Type::Map:             new (&m) shared<map>     (std::move(src.m));  break; \
    case Type::Vector:          new (&v) shared<vector>  (std::move(src.v));  break; \
    case Type::Function:        new (&f) shared<fn>      (std::move(src.f));  break; \
    case Type::EffFunction:     new (&e) shared<eff_fn>  (std::move(src.e));  break; \
    case Type::Thunk:           new (&t) shared<thunk>   (std::move(src.t));  break; \
    case Type::Pointer:         new (&p) shared<void>    (std::move(src.p));  break; \
    \
    default: assert(false && "Unsupported type in move"); \
  }

any::any(const any& other) {
  copy_data(other);
}

any::any(any&& other) noexcept {
  move_data(other);
}

auto any::operator=(const any& rhs) -> any& {
  copy_data(rhs);
  return *this;
}

// Takes ownership -- might need to reconsider this
auto any::operator=(any& rhs) -> any& {
  move_data(rhs);
  return *this;
}

auto any::operator=(any&& rhs) noexcept -> any& {
  move_data(rhs);
  return *this;
}

any::~any() {
  // std::cout << "destroy" << std::endl;
  switch (type) {
    case Type::Integer:         ;                     break;
    case Type::Double:          ;                     break;
    case Type::Character:       ;                     break;
    case Type::Boolean:         ;                     break;
    case Type::String:          s.~shared<string>();  break;
    case Type::Map:             m.~shared<map>();     break;
    case Type::Vector:          v.~shared<vector>();  break;
    case Type::Function:        f.~shared<fn>();      break;
    case Type::EffFunction:     e.~shared<eff_fn>();  break;
    case Type::Thunk:           t.~shared<thunk>();   break;
    case Type::Pointer:         p.~shared<void>();    break;

    default: assert(false && "Unsupported type in destructor");
  }
};

  #define RETURN_VALUE(TYPE, VAL, FN) \
    if (type == TYPE) { \
      return FN(VAL); \
    } \
    const any* valuePtr = this; \
    do { \
      assert(valuePtr->type == Type::Thunk); \
      const any& value = (*valuePtr->t)(unthunk); \
      if (value.type == TYPE) { \
        return FN(value.VAL); \
      } \
      valuePtr = &value; \
    } while (valuePtr->type != Type::Unknown); \
    assert(false && "Unknown value type"); \
    return FN(VAL);

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
  auto any::cast() const -> typename std::enable_if<std::is_same<T, const char*>::value, const T>::type {
    RETURN_VALUE(Type::String, s->c_str(),)
  }
  template auto any::cast<const char*>() const -> const char* const;

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

  auto any::operator()(const any& arg) const -> any {
    if (type == Type::Function) {
      return (*f)(arg);
    }
    const any* valuePtr = this;
    do {
      assert(valuePtr->type == Type::Thunk);
      const any& value = (*valuePtr->t)(unthunk);
      if (value.type == Type::Function) {
        return (*value.f)(arg);
      }
      valuePtr = &value;
    } while (valuePtr->type != Type::Unknown);
    assert(false && "Unknown value type");
    return nullptr;
  }

  auto any::operator()(const as_thunk) const -> const any& {
    assert(type == Type::Thunk);
    return (*t)(unthunk);
  }

  auto any::operator()() const -> any {
    if (type == Type::EffFunction) {
      return (*e)();
    }
    const any* valuePtr = this;
    do {
      assert(valuePtr->type == Type::Thunk);
      const any& value = (*valuePtr->t)(unthunk);
      if (value.type == Type::EffFunction) {
        return (*value.e)();
      }
      valuePtr = &value;
    } while (valuePtr->type != Type::Unknown);
    assert(false && "Unknown value type");
    return nullptr;
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

  auto any::extractPointer() const -> void* {
    RETURN_VALUE(Type::Pointer, p.get(),)
  }

  auto any::extract_value(const any& a) -> const any& {
    if (a.type != Type::Thunk) {
      return a;
    }
    const any* valuePtr = &a;
    do {
      const any& value = (*valuePtr->t)(unthunk);
      if (value.type != Type::Thunk) {
        return value;
      }
      valuePtr = &value;
    } while (valuePtr->type != Type::Unknown);
    assert(false && "Unknown value type");
    return a;
  }

  auto any::operator[](const map_key_t& rhs) const -> const any& {
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
  const any& rhs = any::extract_value(rhs_); \
  assert(rhs.type == any::Type::K); \
  return lhs OP P(rhs.V); \
}

#define DEFINE_OPERATOR_RHS(K, T, OP, V, R, P) \
auto any::operator OP(const T rhs) const -> R { \
  const any& lhs = any::extract_value(*this); \
  assert(lhs.type == Type::K); \
  return P(lhs.V) OP rhs; \
}

auto any::operator==(const any& rhs_) const -> bool {
  const any& lhs = extract_value(*this);
  const any& rhs = extract_value(rhs_);
  assert(lhs.type == rhs.type);
  switch (lhs.type) {
    case Type::Integer:   return lhs.i == rhs.i;
    case Type::Double:    return lhs.d == rhs.d;
    case Type::Character: return lhs.c == rhs.c;
    case Type::Boolean:   return lhs.b == rhs.b;
    case Type::String:    return (*lhs.s) == (*rhs.s);
    case Type::Pointer:   return lhs.p == rhs.p;
    default: assert(false && "Unsupported type for '==' operator");
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
  const any& lhs = extract_value(*this);
  const any& rhs = extract_value(rhs_);
  assert(lhs.type == rhs.type);
  switch (lhs.type) {
    case Type::Integer:   return lhs.i != rhs.i;
    case Type::Double:    return lhs.d != rhs.d;
    case Type::Character: return lhs.c != rhs.c;
    case Type::Boolean:   return lhs.b != rhs.b;
    case Type::String:    return (*lhs.s) != (*rhs.s);
    case Type::Pointer:   return lhs.p != rhs.p;
    default: assert(false && "Unsupported type for '!=' operator");
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
  const any& lhs = extract_value(*this);
  const any& rhs = extract_value(rhs_);
  assert(lhs.type == rhs.type);
  switch (lhs.type) {
    case Type::Integer:   return lhs.i < rhs.i;
    case Type::Double:    return lhs.d < rhs.d;
    case Type::Character: return lhs.c < rhs.c;
    case Type::Boolean:   return lhs.b < rhs.b;
    case Type::String:    return (*lhs.s) < (*rhs.s);
    default: assert(false && "Unsupported type for '<' operator");
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
  const any& lhs = extract_value(*this);
  const any& rhs = extract_value(rhs_);
  assert(lhs.type == rhs.type);
  switch (lhs.type) {
    case Type::Integer:   return lhs.i <= rhs.i;
    case Type::Double:    return lhs.d <= rhs.d;
    case Type::Character: return lhs.c <= rhs.c;
    case Type::Boolean:   return lhs.b <= rhs.b;
    case Type::String:    return (*lhs.s) <= (*rhs.s);
    default: assert(false && "Unsupported type for '<' operator");
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
  const any& lhs = extract_value(*this);
  const any& rhs = extract_value(rhs_);
  assert(lhs.type == rhs.type);
  switch (lhs.type) {
    case Type::Integer:   return lhs.i > rhs.i;
    case Type::Double:    return lhs.d > rhs.d;
    case Type::Character: return lhs.c > rhs.c;
    case Type::Boolean:   return lhs.b > rhs.b;
    case Type::String:    return (*lhs.s) > (*rhs.s);
    default: assert(false && "Unsupported type for '>' operator");
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
  const any& lhs = extract_value(*this);
  const any& rhs = extract_value(rhs_);
  assert(lhs.type == rhs.type);
  switch (lhs.type) {
    case Type::Integer:   return lhs.i >= rhs.i;
    case Type::Double:    return lhs.d >= rhs.d;
    case Type::Character: return lhs.c >= rhs.c;
    case Type::Boolean:   return lhs.b >= rhs.b;
    case Type::String:    return (*lhs.s) >= (*rhs.s);
    default: assert(false && "Unsupported type for '>' operator");
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
  const any& lhs = extract_value(*this);
  const any& rhs = extract_value(rhs_);
  assert(lhs.type == rhs.type);
  switch (lhs.type) {
    case Type::Integer:   return lhs.i + rhs.i;
    case Type::Double:    return lhs.d + rhs.d;
    case Type::Character: return any(char(lhs.c + rhs.c));
    case Type::String:    return (*lhs.s) + (*rhs.s);
    default: assert(false && "Unsupported type for '+' operator");
  }
  return nullptr;
}

DEFINE_OPERATOR_RHS(Integer,   long,        +, i, long,)
DEFINE_OPERATOR_RHS(Double,    double,      +, d, double,)
DEFINE_OPERATOR_RHS(Character, char,        +, c, char,)
DEFINE_OPERATOR_RHS(String,    string&,     +, s, string, *)
DEFINE_OPERATOR_RHS(String,    char* const, +, s, string, *)

auto any::operator-(const any& rhs_) const -> any {
  const any& lhs = extract_value(*this);
  const any& rhs = extract_value(rhs_);
  assert(lhs.type == rhs.type);
  switch (lhs.type) {
    case Type::Integer:   return lhs.i - rhs.i;
    case Type::Double:    return lhs.d - rhs.d;
    case Type::Character: return any(char(lhs.c - rhs.c));
    default: assert(false && "Unsupported type for '-' binary operator");
  }
  return nullptr;
}

DEFINE_OPERATOR_RHS(Integer,   long,   -, i, long,)
DEFINE_OPERATOR_RHS(Double,    double, -, d, double,)
DEFINE_OPERATOR_RHS(Character, char,   -, c, char,)

auto any::operator*(const any& rhs_) const -> any {
  const any& lhs = extract_value(*this);
  const any& rhs = extract_value(rhs_);
  assert(lhs.type == rhs.type);
  switch (lhs.type) {
    case Type::Integer: return lhs.i * rhs.i;
    case Type::Double:  return lhs.d * rhs.d;
    default: assert(false && "Unsupported type for '*' operator");
  }
  return nullptr;
}

DEFINE_OPERATOR_RHS(Integer,   long,   *, i, long,)
DEFINE_OPERATOR_RHS(Double,    double, *, d, double,)

auto any::operator/(const any& rhs_) const -> any {
  const any& lhs = extract_value(*this);
  const any& rhs = extract_value(rhs_);
  assert(lhs.type == rhs.type);
  switch (lhs.type) {
    case Type::Integer: return lhs.i / rhs.i;
    case Type::Double:  return lhs.d / rhs.d;
    default: assert(false && "Unsupported type for '/' operator");
  }
  return nullptr;
}

DEFINE_OPERATOR_RHS(Integer,   long,   /, i, long,)
DEFINE_OPERATOR_RHS(Double,    double, /, d, double,)

auto any::operator%(const any& rhs_) const -> any {
  const any& lhs = extract_value(*this);
  const any& rhs = extract_value(rhs_);
  assert(lhs.type == rhs.type);
  switch (lhs.type) {
    case Type::Integer: return lhs.i % rhs.i;
    default: assert(false && "Unsupported type for '%' operator");
  }
  return nullptr;
}

DEFINE_OPERATOR_RHS(Integer,   long, %, i, long,)

auto any::operator-() const -> any {
  const any& lhs = any::extract_value(*this);
  switch (lhs.type) {
    case Type::Integer: return (- lhs.i);
    case Type::Double:  return (- lhs.d);
    default: assert(false && "Unsupported type for unary '-' operator");
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
