///////////////////////////////////////////////////////////////////////////////
//
// Module      :  purescript.h
// Copyright   :  (c) Andy Arvanitis 2018
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
#ifndef purescript_H
#define purescript_H

#include <memory>
#include <vector>
#include <string>
#include <utility>
#if !defined(NDEBUG)
#include <limits>
#include <stdexcept>
#endif
#include "functions.h"
#include "dictionary.h"
#include "recursion.h"

namespace purescript {

    using std::string;

    class boxed {
    public:
        std::shared_ptr<void> shared;
        union {
            int _int_;
            double _double_;
            bool _bool_;
        };

    public:
        using fn_t = _template_::fn_t<boxed>;
        using eff_fn_t = _template_::eff_fn_t<boxed>;

        using dict_t = _template_::dict_t<boxed>;
        using array_t = std::vector<boxed>;

        using weak = _template_::weak<boxed>;
        using recur = _template_::recur<boxed>;

    private:
        template <typename T>
        using fn_T = _template_::fn_T<boxed, T>;

        template <typename T>
        using eff_fn_T = _template_::eff_fn_T<boxed, T>;

    public:
        boxed() noexcept : shared() {}
        boxed(const std::nullptr_t) noexcept : shared() {}

        boxed(const weak& w) : shared(w.shared()) {}
        boxed(const recur& r) : shared(r.shared()) {}
        boxed(const recur::weak& w) : shared(w.shared()) {}

        template <typename T>
        boxed(std::shared_ptr<T>&& other) noexcept : shared(std::move(other)) {}

        template <typename T>
        boxed(const std::shared_ptr<T>& other) : shared(other) {}

        boxed(const int n) noexcept : _int_(n) {}
        boxed(const long n);
        boxed(const unsigned long n);
        boxed(const double n) noexcept : _double_(n) {}
        boxed(const bool b) noexcept : _bool_(b) {}
        boxed(const char s[]) : shared(std::make_shared<string>(s)) {}
        boxed(string&& s) : shared(std::make_shared<string>(std::move(s))) {}
        boxed(const string& s) : shared(std::make_shared<string>(s)) {}
        boxed(array_t&& l) : shared(std::make_shared<array_t>(std::move(l))) {}
        boxed(const array_t& l) : shared(std::make_shared<array_t>(l)) {}
        boxed(dict_t&& m) : shared(std::make_shared<dict_t>(std::move(m))) {}
        boxed(const dict_t& m) : shared(std::make_shared<dict_t>(m)) {}

        template <typename T,
                  typename = typename std::enable_if<!std::is_same<boxed,T>::value>::type>
        boxed(T f,
              typename std::enable_if<std::is_same<decltype(std::declval<T>()(std::declval<boxed>())),
                                                   boxed>::value>::type* = 0)
              : shared(std::shared_ptr<fn_t>(std::make_shared<fn_T<T>>(std::move(f)))) {
        }

        template <typename T,
                  typename = typename std::enable_if<!std::is_same<boxed,T>::value>::type>
        boxed(T f,
              typename std::enable_if<std::is_same<decltype(std::declval<T>()()),
                                                   boxed>::value>::type* = 0)
              : shared(std::shared_ptr<eff_fn_t>(std::make_shared<eff_fn_T<T>>(std::move(f)))) {
        }

        auto get() const noexcept -> void * {
            return shared.get();
        }

        auto operator()(const boxed& arg) const -> boxed {
            return (*static_cast<fn_t*>(shared.get()))(arg);
        }

        auto operator()() const -> boxed {
            return (*static_cast<eff_fn_t*>(shared.get()))();
        }

        auto operator[](const char key[]) const -> const boxed& {
          return (*static_cast<const dict_t*>(shared.get()))[key];
        }

        auto operator[](const char key[]) -> boxed& {
          return (*static_cast<dict_t*>(shared.get()))[key];
        }

        auto operator[](const int index) const -> const boxed&;
        auto operator[](const int index) -> boxed&;

    }; // class boxed

    extern template class _template_::fn_t<boxed>;
    extern template class _template_::eff_fn_t<boxed>;
    extern template class _template_::dict_t<boxed>;
    extern template class _template_::weak<boxed>;
    extern template class _template_::recur<boxed>;

    extern const boxed undefined;

    using fn_t = boxed::fn_t;
    using eff_fn_t = boxed::eff_fn_t;
    using dict_t = boxed::dict_t;
    using array_t = boxed::array_t;

    template <typename T, typename... Args,
              typename = typename std::enable_if<!std::is_same<T, int>::value &&
                                                 !std::is_same<T, double>::value &&
                                                 !std::is_same<T, bool>::value
                                                >::type>
    inline auto box(Args&&... args) -> boxed {
        return std::make_shared<T>(std::forward<Args>(args)...);
    }

    template <typename T,
              typename std::enable_if<std::is_same<T, int>::value, T>::type* = nullptr>
    inline auto box(T arg) noexcept -> boxed {
        return boxed(arg);
    }

    template <typename T,
              typename std::enable_if<std::is_same<T, double>::value, T>::type* = nullptr>
    inline auto box(T arg) noexcept -> boxed {
        return boxed(arg);
    }

    template <typename T,
              typename std::enable_if<std::is_same<T, bool>::value, T>::type* = nullptr>
    inline auto box(T arg) noexcept -> boxed {
        return boxed(arg);
    }

    template <typename T,
              typename = typename std::enable_if<!std::is_same<T, int>::value &&
                                                 !std::is_same<T, double>::value &&
                                                 !std::is_same<T, bool>::value
                                                >::type>
    constexpr auto unbox(const boxed& b) -> const T& {
        return *static_cast<const T*>(b.get());
    }

    template <typename T,
              typename std::enable_if<std::is_same<T, int>::value, T>::type* = nullptr>
    constexpr auto unbox(const boxed& b) noexcept -> T {
        return b._int_;
    }

    template <typename T,
              typename std::enable_if<std::is_same<T, double>::value, T>::type* = nullptr>
    constexpr auto unbox(const boxed& b) noexcept -> T {
        return b._double_;
    }

    template <typename T,
              typename std::enable_if<std::is_same<T, bool>::value, T>::type* = nullptr>
    constexpr auto unbox(const boxed& b) noexcept -> T {
        return b._bool_;
    }

    template <typename T,
              typename = typename std::enable_if<!std::is_same<T, int>::value &&
                                                 !std::is_same<T, double>::value &&
                                                 !std::is_same<T, bool>::value
                                                >::type>
    constexpr auto unbox(boxed& b) -> T& {
        return *static_cast<T*>(b.get());
    }

    template <typename T>
    constexpr auto unbox(const T value) noexcept -> T {
        return value;
    }

    template <typename T,
              typename = typename std::enable_if<std::is_same<T, int>::value>::type>
    constexpr auto unbox(const std::size_t value) noexcept -> long long {
        return value;
    }

    inline auto array_length(const boxed& a) -> boxed::array_t::size_type {
        return unbox<boxed::array_t>(a).size();
    }

} // namespace purescript

#define DEFINE_FOREIGN_DICTIONARY_AND_ACCESSOR() \
    inline auto foreign() -> dict_t& {\
        static dict_t _;\
        return _;\
    }

#define FOREIGN_BEGIN(NS) namespace NS {\
    using namespace purescript;\
    DEFINE_FOREIGN_DICTIONARY_AND_ACCESSOR()\
    static const auto data = ([]() -> char {\
        dict_t& exports = foreign();
#define FOREIGN_END return 0; }()); }

#endif // purescript_H
