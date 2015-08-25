///////////////////////////////////////////////////////////////////////////////
//
// Module      :  constructor.hh
// Copyright   :  (c) Andy Arvanitis 2015
// License     :  MIT
//
// Maintainer  :  Andy Arvanitis <andy.arvanitis@gmail.com>
// Stability   :  experimental
// Portability :
//
// Types and functions to support partial application of data constructors
//
///////////////////////////////////////////////////////////////////////////////
//
#ifndef PS_constructor_HH
#define PS_constructor_HH

#include "memory.hh"

namespace PureScript {

  namespace Private {
    template <typename CtorType>
    struct CtorHelper {
      template <typename ParamType, typename ...ParamTypes, typename ...ArgTypes>
      static constexpr auto curry(ArgTypes ...args) {
        return [=](ParamType param) {
          return curry<ParamTypes...>(args..., param);
        };
      }
      template <typename ...ArgTypes>
      static constexpr auto curry(ArgTypes ...args) {
        return construct<CtorType>(args...);
      }
    };
  }

  template <typename CtorType, typename ...ParamTypes, typename ...ArgTypes>
  constexpr auto constructor(ArgTypes ...args) {
    return Private::CtorHelper<CtorType>::template curry<ParamTypes...>(args...);
  }

  template <typename CtorType>
  constexpr auto constructor() {
    return [=]() {
      return construct<CtorType>();
    };
  }

} // namespace PureScript

#endif // PS_constructor_HH
