///////////////////////////////////////////////////////////////////////////////
//
// Module      :  purescript.cpp
// Copyright   :  (c) Andy Arvanitis 2019
// License     :  BSD
//
// Maintainer  :  Andy Arvanitis
// Stability   :  experimental
// Portability :
//
// Basic types and functions to support purescript-to-C++ rendering
//
///////////////////////////////////////////////////////////////////////////////
//
#include "purescript.h"

namespace purescript {

template class _template_::fn_t<boxed>;
template class _template_::eff_fn_t<boxed>;
template class _template_::dict_t<boxed>;
template class _template_::weak<boxed>;
template class _template_::recur<boxed>;

boxed::boxed(const long n) : _int_(static_cast<int>(n)) {
#if !defined(NDEBUG)
    if (n < std::numeric_limits<int>::min() || n > std::numeric_limits<int>::max()) {
        throw std::runtime_error("integer out of range");
    }
#endif
}

boxed::boxed(const unsigned long n) : _int_(static_cast<int>(n)) {
#if !defined(NDEBUG)
    if (n > std::numeric_limits<int>::max()) {
        throw std::runtime_error("integer out of range");
    }
#endif
}

auto boxed::operator[](const int index) const -> const boxed& {
#if !defined(NDEBUG)
    return static_cast<const array_t*>(shared.get())->at(index);
#else
    return (*static_cast<const array_t*>(shared.get()))[index];
#endif
}

auto boxed::operator[](const int index) -> boxed& {
#if !defined(NDEBUG)
    return static_cast<array_t*>(shared.get())->at(index);
#else
    return (*static_cast<array_t*>(shared.get()))[index];
#endif
}

const boxed undefined;

} // namespace purescript
