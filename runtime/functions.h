///////////////////////////////////////////////////////////////////////////////
//
// Module      :  functions.h
// Copyright   :  (c) Andy Arvanitis 2019
// License     :  BSD
//
// Maintainer  :  Andy Arvanitis
// Stability   :  experimental
// Portability :
//
// Runtime function types (for regular functions and lambdas)
//
///////////////////////////////////////////////////////////////////////////////
//
#ifndef purescript_functions_H
#define purescript_functions_H

namespace purescript {
    namespace _template_ {

    template <typename T>
    class fn_t {
    public:
        virtual ~fn_t() {}
        virtual auto operator ()(const T&) const -> T = 0;
    };

    template <typename T, typename U>
    class fn_T : public fn_t<T> {
        U fn;
    public:
        fn_T(U f) noexcept : fn(std::move(f)) {}
        auto operator ()(const T& arg) const -> T override {
            return fn(arg);
        }
    };

    template <typename T>
    class eff_fn_t {
    public:
        virtual ~eff_fn_t() {}
        virtual auto operator ()() const -> T = 0;
    };

    template <typename T, typename U>
    class eff_fn_T : public eff_fn_t<T> {
        U fn;
    public:
        eff_fn_T(U f) noexcept : fn(std::move(f)) {}
        auto operator ()() const -> T override {
            return fn();
        }
    };

    } // namespace _template_
} // namespace purescript

#endif // purescript_functions_H
