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

#include "hash.hh"

namespace PureScript {

  class map_key_t {

    const uint32_t hash;

    public:
    explicit constexpr map_key_t(const uint32_t hash) : hash(hash) {}

    class hasher {
    public:
      constexpr auto operator() (const map_key_t key) const -> size_t {
        return key.hash;
      }
    };

    class equal {
    public:
      constexpr auto operator() (const map_key_t key1, const map_key_t key2) const -> bool {
        return key1.hash == key2.hash;
      }
    };
  };

  // Compile-time string-key literals
  //
  constexpr auto operator "" _key(const char s[], size_t) -> const map_key_t {
    return map_key_t(djb2(s));
  }

} // namespace PureScript

#endif // PS_MapKey_HH
