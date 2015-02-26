#ifndef Pure11_H
#define Pure11_H

// Standard includes
//
#include <functional>
#include <memory>
#include <string>
#include <iostream>
#include "shared_list.hh"

// Type support

template <typename T, typename Enable = void>
struct ADT;

template <typename T>
struct ADT <T, typename std::enable_if<std::is_fundamental<T>::value>::type> {
  using type = T;
  template <typename... ArgTypes>
  constexpr static auto make(ArgTypes... args) -> type {
    return T(args...);
  }
};

template <typename T>
struct ADT <T, typename std::enable_if<!std::is_fundamental<T>::value>::type> {
  using type = std::shared_ptr<T>;
  template <typename... ArgTypes>
  constexpr static auto make(ArgTypes... args) -> type {
    return std::make_shared<T>(args...);
  }
};

// Type aliases
//
template <typename A, typename B> using fn = std::function<B(A)>;
template <typename T> using data = typename ADT<T>::type;
template <typename T> using list = shared_list<T>;
using list_index_type = list<void*>::size_type;
using string = std::string;
template <typename B> using eff_fn = std::function<B()>;

// Function aliases

template <typename T, typename... ArgTypes>
constexpr auto make_data(ArgTypes... args) -> typename ADT<T>::type {
  return ADT<T>::make(args...);
}

template <typename T, typename U>
constexpr auto cast(const std::shared_ptr<U>& a) -> T {
  return *(std::dynamic_pointer_cast<T>(a));
}

template <typename T, typename U>
constexpr auto instanceof(const std::shared_ptr<U>& a) -> std::shared_ptr<T> {
  return std::dynamic_pointer_cast<T>(a);
}

#endif // Pure11_H
