///////////////////////////////////////////////////////////////////////////////
//
// Module      :  hash.hh
// Copyright   :  (c) Andy Arvanitis 2015
// License     :  MIT
//
// Maintainer  :  Andy Arvanitis <andy.arvanitis@gmail.com>
// Stability   :  experimental
// Portability :
//
// Provides compile-time hashes for string literals.
//
///////////////////////////////////////////////////////////////////////////////
//
#ifndef PS_Hash_HH
#define PS_Hash_HH

#include <cstdint>

namespace PureScript {

  // Hash function which uses an xor version of the djb2 algorithm
  //
  constexpr auto djb2(const char s[], const uint32_t hash = 5381) -> uint32_t {
    return !s[0] ? hash : djb2(s + 1, 33 * hash ^ s[0]);
  }

} // namespace PureScript

#endif // PS_Hash_HH
