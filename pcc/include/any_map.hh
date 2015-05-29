///////////////////////////////////////////////////////////////////////////////
//
// Module      :  any_map.hh
// Copyright   :  (c) Andy Arvanitis 2015
// License     :  MIT
//
// Maintainer  :  Andy Arvanitis <andy.arvanitis@gmail.com>
// Stability   :  experimental
// Portability :
//
// Type to represent a hash map that can hold any data type
//
///////////////////////////////////////////////////////////////////////////////
//
#ifndef any_map_H_
#define any_map_H_

#include <string>
#include <unordered_map>
#include "memory.hh"

namespace PureScript {

using any_map = std::unordered_map<std::string, const unsafe_any>;

} // namespace PureScript

#endif // any_map_H_
