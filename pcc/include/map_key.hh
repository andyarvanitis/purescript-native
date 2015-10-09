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

  struct map_key_t {

    const uint32_t hash;

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

  // Compile-time map keys (from strings)
  //
  template <uint32_t H>
  struct _compile_time_key_ {
    static constexpr map_key_t value = map_key_t(H);
  };

  template <uint32_t H>
  constexpr map_key_t _compile_time_key_<H>::value;

  #define KEY(s) _compile_time_key_<djb2(s)>::value

} // namespace PureScript

#endif // PS_MapKey_HH
