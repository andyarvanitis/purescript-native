///////////////////////////////////////////////////////////////////////////////
//
// Module      :  map_key.hh
// Copyright   :  (c) Andy Arvanitis 2015
// License     :  MIT
//
// Maintainer  :  Andy Arvanitis <andy.arvanitis@gmail.com>
// Stability   :  experimental
// Portability :
//
// Support for maps using keys with compile-time hashes.
//
///////////////////////////////////////////////////////////////////////////////
//
#ifndef PS_MapKey_HH
#define PS_MapKey_HH

#include <cstring>

namespace PureScript {

  struct map_key_t {

    const uint64_t hash;
    const char * const cstr;

    // Compile-time (fnv-1a) hash function
    static constexpr auto fnv1a(const char s[], const uint64_t hash = 14695981039346656037UL) -> uint64_t {
      return !s[0] ? hash : fnv1a(s + 1, (hash ^ static_cast<unsigned char>(s[0])) * 1099511628211UL);
    }

    class hasher {
    public:
      auto operator() (const map_key_t& key) const -> size_t {
        return key.hash;
      }
    };

    class equal {
    public:
      auto operator() (const map_key_t& key1, const map_key_t& key2) const -> bool {
        return (key1.cstr == key2.cstr) || strcmp(key1.cstr, key2.cstr) == 0;
      }
    };
  };

  template <uint64_t H>
  constexpr auto make_map_key(const char s[]) -> const map_key_t {
    return map_key_t{H, s};
  }

  #define KEY(k) make_map_key<map_key_t::fnv1a(k)>(k)

} // namespace PureScript

#endif // PS_MapKey_HH
