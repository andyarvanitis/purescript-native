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

#include <utility>
#include "hash.hh"

namespace PureScript {

  class map_key_t : public std::pair<uint32_t, std::string> {
  public:
    using std::pair<uint32_t, std::string>::pair;

    class hasher {
    public:
      auto operator() (const map_key_t& key) const -> size_t {
        return key.first;
      }
    };

    class equal {
    public:
      auto operator() (const map_key_t& key1, const map_key_t& key2) const -> bool {
        return key1.second == key2.second;
      }
    };
  };

  #define KEY(s) map_key_t(HASH(s), s)

} // namespace PureScript

#endif // PS_MapKey_HH
