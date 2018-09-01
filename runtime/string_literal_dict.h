#ifndef string_literal_dict_H
#define string_literal_dict_H

#include <cstring>
#include <string>
#include <vector>
#include <utility>


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
#ifndef NDEBUG
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
