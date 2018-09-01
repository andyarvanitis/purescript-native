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
#include <functional>
#include <deque>
#include <string>
#include <limits>
#include "string_literal_dict.h"


namespace purescript {

    using std::string;

    class boxed : public std::shared_ptr<void> {
        
    public:
        using fn_t = std::function<boxed(const boxed&)>;
        using eff_fn_t = std::function<boxed(void)>;
        using dict_t = string_literal_dict_t<boxed>;
        using array_t = std::deque<boxed>;

    public:
        using std::shared_ptr<void>::shared_ptr;

        boxed() noexcept : std::shared_ptr<void>() {
        }

        boxed(const std::nullptr_t) noexcept : std::shared_ptr<void>() {
        }

        boxed(const int n) : std::shared_ptr<void>(std::make_shared<int>(n)) {
        }

        boxed(const long n) : std::shared_ptr<void>(std::make_shared<int>(static_cast<int>(n))) {
#ifndef NDEBUG
            if (n < std::numeric_limits<int>::min() || n > std::numeric_limits<int>::max()) {
                throw std::runtime_error("integer out of range");
            }
#endif // !NDEBUG
        }

        boxed(const double n) : std::shared_ptr<void>(std::make_shared<double>(n)) {
        }

        boxed(const bool b) : std::shared_ptr<void>(std::make_shared<bool>(b)) {
        }

        boxed(const char s[]) : std::shared_ptr<void>(std::make_shared<string>(s)) {
        }

        boxed(string&& s) : std::shared_ptr<void>(std::make_shared<string>(std::move(s))) {
        }

        boxed(array_t&& l) : std::shared_ptr<void>(std::make_shared<array_t>(std::move(l))) {
        }

        boxed(dict_t&& m) : std::shared_ptr<void>(std::make_shared<dict_t>(std::move(m))) {
        }

        template <typename T,
                  typename = typename std::enable_if<!std::is_same<boxed,T>::value>::type>
        boxed(const T& f,
              typename std::enable_if<std::is_assignable<std::function<boxed(boxed)>,T>::value>::type* = 0) : std::shared_ptr<void>(std::make_shared<fn_t>(f)) {
        }

        template <typename T,
                  typename = typename std::enable_if<!std::is_same<boxed,T>::value>::type>
        boxed(const T& f,
              typename std::enable_if<std::is_assignable<std::function<boxed(void)>,T>::value>::type* = 0) : std::shared_ptr<void>(std::make_shared<eff_fn_t>(f)) {
        }

        template <typename T,
        typename = typename std::enable_if<!std::is_same<boxed,T>::value &&
                                            std::is_convertible<T,fn_t>::value>::type>
        auto operator=(const T& right) -> boxed& {
            if (std::shared_ptr<void>::operator bool()) {
                auto& f = *static_cast<fn_t*>(get());
                f = right;
            } else {
                reset(new fn_t(right));
            }
            return *this;
        }

        auto operator()(const boxed& arg) const -> boxed {
            auto& f = *static_cast<fn_t*>(get());
            return f(arg);
        }

        auto operator()() const -> boxed {
            auto& f = *static_cast<eff_fn_t*>(get());
            return f();
        }

        auto operator[](const char key[]) const -> const boxed& {
          const auto& dict = *static_cast<const dict_t*>(get());
          return dict[key];
        }

        auto operator[](const char key[]) -> boxed& {
          auto& dict = *static_cast<dict_t*>(get());
          return dict[key];
        }

#ifdef NDEBUG
        auto operator[](const int index) const -> const boxed& {
          const auto& array = *static_cast<const array_t*>(get());
          return array[index];
        }

        auto operator[](const int index) -> boxed& {
          auto& array = *static_cast<array_t*>(get());
          return array[index];
        }
#else
        auto operator[](const int index) const -> const boxed& {
          const auto& array = *static_cast<const array_t*>(get());
          return array.at(index);
        }

        auto operator[](const int index) -> boxed& {
          auto& array = *static_cast<array_t*>(get());
          return array.at(index);
        }
#endif // NDEBUG

    }; // class boxed

    template <typename T, typename... Args>
    inline auto box(Args&&... args) -> boxed {
        return std::make_shared<T>(std::forward<Args>(args)...);
    }

    template <typename T>
    inline auto unbox(const boxed& b) -> const T& {
        const auto& unboxed = *static_cast<const T*>(b.get());
        return unboxed;
    }

    template <typename T,
              typename = typename std::enable_if<!std::is_same<T, const boxed&>::value>::type>
    constexpr auto unbox(const T value) -> T {
        return value;
    }

    template <typename T,
        typename = typename std::enable_if<!std::is_same<T, const boxed&>::value &&
                                            std::is_same<T, int>::value>::type>
    constexpr auto unbox(const std::size_t value) -> long long {
        return value;
    }

    template <typename T>
    inline auto peek(boxed& b) -> T& {
        auto& unboxed = *static_cast<T*>(b.get());
        return unboxed;
    }

    template <typename T>
    inline auto copy(const boxed& b) -> boxed {
        return box<T>(unbox<T>(b));
    }

    using fn_t = boxed::fn_t;
    using eff_fn_t = boxed::eff_fn_t;
    using dict_t = boxed::dict_t;
    using array_t = boxed::array_t;

    constexpr auto undefined = nullptr;

} // namespace purescript

#define FOREIGN_BEGIN(NS) namespace NS {\
    using namespace purescript;\
    auto foreign() -> dict_t&;\
    static const auto& NS ## $foreign_init = []() {\
        dict_t& exports = foreign();
#define FOREIGN_END return exports; }(); }

#endif // purescript_H
