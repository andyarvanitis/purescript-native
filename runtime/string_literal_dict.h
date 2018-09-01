///////////////////////////////////////////////////////////////////////////////
//
// Module      :  string_literal_dict.h
// Copyright   :  (c) Andy Arvanitis 2018
// License     :  BSD
//
// Maintainer  :  Andy Arvanitis
// Stability   :  experimental
// Portability :
//
// A simple dictionary optimized for this runtime's particular use –
// *not* intended for general use or as a replacement for containers
// such as std::map or std::unordered_map. It relies on the following
// assumptions:
//
//    1) Keys are C-type string literals. No copying or (memory management)
//       of the keys occurs
//    2) The number of elements in the container is generally small – say,
//       under 20. There is no sorting and a linear search is used to
//       locate elements
//
///////////////////////////////////////////////////////////////////////////////
//
#ifndef string_literal_dict_H
#define string_literal_dict_H

#include <cstring>
#include <vector>
#include <utility>
#if !defined(NDEBUG)
#include <string>
#endif

template <typename T>
class string_literal_dict_t : private std::vector<std::pair<const char *, T>> {

  static auto null() -> const T& {
    static const T value;
    return value;
  }

  public:
  using std::vector<std::pair<const char *, T>>::vector;

  auto operator[](const char key[]) const -> const T& {
    for (auto it = this->cbegin(), end=this->cend(); it != end; it++) {
      const auto& elem_key = it->first;
      if (elem_key == key || !std::strcmp(elem_key, key)) {
        return it->second;
      }
    }
#if !defined(NDEBUG) // if debug build
    throw std::runtime_error("dictionary key \"" + std::string(key) + "\" not found");
#endif
    return null();
  }

  auto operator[](const char key[]) -> T& {
    const auto end = this->end();
    for (auto it = this->begin(); it != end; it++) {
      const auto& elem_key = it->first;
      if (elem_key == key || !std::strcmp(elem_key, key)) {
        return it->second;
      }
    }
    return this->emplace(end, key, T())->second;
  }

  auto contains(const char key[]) const -> bool {
    for (auto it = this->cbegin(), end=this->cend(); it != end; it++) {
      const auto& elem_key = it->first;
      if (elem_key == key || !std::strcmp(elem_key, key)) {
        return true;
      }
    }
    return false;
  }

};


#endif // string_literal_dict_H
