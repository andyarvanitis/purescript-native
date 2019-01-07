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

#include <functional>
#include <deque>
#include <string>
#include <limits>
#include <gc/gc_cpp.h>
#include <gc/gc_allocator.h>
#include "string_literal_dict.h"


namespace purescript {

    template <typename T>
    class collected : public T, public gc_cleanup {
        public:
        template <typename... Args>
        inline collected(Args&&... args) : T(std::forward<Args>(args)...), gc_cleanup() {
        }
    };

    using string = collected<std::string>;

    class boxed {

    private:
        union {
            void * _ptr_;
            int _int_;
            bool _bool_;
            double _double_;
        };

    private:
        template <typename T>
        class collected_array : public std::deque<T, gc_allocator<T>>, public gc {
            public:

            collected_array(std::initializer_list<T> init)
                : std::deque<T, gc_allocator<T>>(init)
                , gc() {
            }

            template <typename... Args>
            inline collected_array(Args&&... args)
                : std::deque<T, gc_allocator<T>>(std::forward<Args>(args)...),
                  gc() {
            }
        };

        using dict_value_t = string_literal_dict_t<boxed>::value_type;
        class collected_dict : public string_literal_dict_t<boxed, gc_allocator<dict_value_t>>, public gc {
            public:
            collected_dict(std::initializer_list<dict_value_t> init)
                : string_literal_dict_t<boxed, gc_allocator<dict_value_t>>(init)
                , gc() {
            }

            template <typename... Args>
            inline collected_dict(Args&&... args)
                : string_literal_dict_t<boxed, gc_allocator<dict_value_t>>(std::forward<Args>(args)...),
                  gc() {
            }
        };

    public:
        using fn_t = collected<std::function<boxed(const boxed&)>>;
        using eff_fn_t = collected<std::function<boxed(void)>>;
        using dict_t = collected_dict;
        using array_t = collected_array<boxed>;

    public:
        boxed() noexcept : _ptr_(nullptr) {}
        boxed(const std::nullptr_t) noexcept : _ptr_(nullptr) {}
        explicit boxed(void * p) noexcept : _ptr_(p) {}
        boxed(const int n) : _int_(n) {}
        boxed(const long n) : _int_(static_cast<int>(n)) {
#if !defined(NDEBUG) // if debug build
            if (n < std::numeric_limits<int>::min() || n > std::numeric_limits<int>::max()) {
                throw std::runtime_error("integer out of range");
            }
#endif // !defined(NDEBUG)
        }
        boxed(const unsigned long n) : _int_(static_cast<int>(n)) {
#if !defined(NDEBUG) // if debug build
            if (n > std::numeric_limits<int>::max()) {
                throw std::runtime_error("integer out of range");
            }
#endif // !defined(NDEBUG)
        }
        boxed(const double n) : _double_(n) {}
        boxed(const bool b) : _bool_(b) {}
        boxed(const char s[]) : _ptr_(new string(s)) {}
        boxed(std::string&& s) : _ptr_(new string(std::move(s))) {}
        boxed(const std::string& s) : _ptr_(new string(s)) {}
        boxed(const string& s) : _ptr_(new string(s)) {}
        // boxed(array_t&& l) : _ptr_(new array_t(std::move(l))) {}
        boxed(const array_t& l) : _ptr_(new array_t(l)) {}
        // boxed(dict_t&& m) : _ptr_(new dict_t(std::move(m))) {}
        boxed(const dict_t& m) : _ptr_(new dict_t(m)) {}

        template <typename T,
                  typename = typename std::enable_if<!std::is_same<boxed,T>::value>::type>
        boxed(const T& f,
              typename std::enable_if<std::is_assignable<std::function<boxed(boxed)>,T>::value>::type* = 0)
              : _ptr_(new fn_t(f)) {
        }

        template <typename T,
                  typename = typename std::enable_if<!std::is_same<boxed,T>::value>::type>
        boxed(const T& f,
              typename std::enable_if<std::is_assignable<std::function<boxed(void)>,T>::value>::type* = 0)
              : _ptr_(new eff_fn_t(f)) {
        }

        auto get() const noexcept -> void * {
            return _ptr_;
        }

        auto getInt() const noexcept -> int {
            return _int_;
        }

        auto getBool() const noexcept -> bool {
            return _bool_;
        }

        auto getDouble() const noexcept -> double {
            return _double_;
        }

        auto operator()(const boxed& arg) const -> boxed {
            auto& f = *static_cast<fn_t*>(_ptr_);
            return f(arg);
        }

        auto operator()() const -> boxed {
            auto& f = *static_cast<eff_fn_t*>(_ptr_);
            return f();
        }

        auto operator[](const char key[]) const -> const boxed& {
          return (*static_cast<const dict_t*>(_ptr_))[key];
        }

        auto operator[](const char key[]) -> boxed& {
          return (*static_cast<dict_t*>(_ptr_))[key];
        }

#if !defined(NDEBUG) // if debug build
        auto operator[](const int index) const -> const boxed& {
            return static_cast<const array_t*>(_ptr_)->at(index);
        }

        auto operator[](const int index) -> boxed& {
            return static_cast<array_t*>(_ptr_)->at(index);
        }
#else  // not debug build
        auto operator[](const int index) const -> const boxed& {
            return (*static_cast<const array_t*>(_ptr_))[index];
        }

        auto operator[](const int index) -> boxed& {
            return (*static_cast<array_t*>(_ptr_))[index];
        }
#endif // !defined(NDEBUG)

    }; // class boxed

    using fn_t = boxed::fn_t;
    using eff_fn_t = boxed::eff_fn_t;
    using dict_t = boxed::dict_t;
    using array_t = boxed::array_t;

    class boxed_r {
        private:
        boxed * _ptr_;

        public:
        boxed_r() noexcept :  _ptr_(new (UseGC) boxed()) {}

        operator const boxed&() const {
            return *static_cast<const boxed*>(_ptr_);
        }

        auto operator()(const boxed& arg) const -> boxed {
            auto& f = *static_cast<fn_t*>(_ptr_->get());
            return f(arg);
        }

        auto operator()() const -> boxed {
            auto& f = *static_cast<eff_fn_t*>(_ptr_->get());
            return f();
        }

        template <typename T>
        auto operator=(T&& right) -> boxed_r& {
            *_ptr_ = std::forward<T>(right);
            return *this;
        }

    }; // class boxed_r

    template <typename T, typename... Args>
    inline auto box(Args&&... args) -> boxed {
        return boxed(static_cast<void*>(new collected<T>(std::forward<Args>(args)...)));
    }

    template <typename T>
    constexpr auto unbox(const boxed& b, typename std::enable_if<std::is_same<T,int>::value>::type* = 0) -> T {
        return b.getInt();
    }

    template <typename T>
    constexpr auto unbox(const boxed& b, typename std::enable_if<std::is_same<T,bool>::value>::type* = 0) -> T {
        return b.getBool();
    }

    template <typename T>
    constexpr auto unbox(const boxed& b, typename std::enable_if<std::is_same<T,double>::value>::type* = 0) -> T {
        return b.getDouble();
    }

    template <typename T,
              typename = typename std::enable_if<!std::is_same<T, int>::value &&
                                                 !std::is_same<T, bool>::value &&
                                                 !std::is_same<T, double>::value>::type>
    constexpr auto unbox(const boxed& b) -> const T& {
        return *static_cast<const T*>(b.get());
    }

    template <typename T>
    constexpr auto unbox(boxed& b) -> T& {
        return *static_cast<T*>(b.get());
    }

    template <typename T>
    constexpr auto unbox(const T value) -> T {
        return value;
    }

    template <typename T,
              typename = typename std::enable_if<std::is_same<T, int>::value>::type>
    constexpr auto unbox(const std::size_t value) -> long long {
        return value;
    }

    inline auto array_length(const boxed& a) -> boxed::array_t::size_type {
        return unbox<boxed::array_t>(a).size();
    }

    constexpr auto undefined = nullptr;

} // namespace purescript

#define DEFINE_FOREIGN_DICTIONARY_AND_ACCESSOR() \
    inline auto foreign() -> dict_t& {\
        static dict_t $dict$;\
        return $dict$;\
    }

#define FOREIGN_BEGIN(NS) namespace NS {\
    using namespace purescript;\
    DEFINE_FOREIGN_DICTIONARY_AND_ACCESSOR()\
    static const auto $foreign_exports_init$ = ([]() -> char {\
        dict_t& exports = foreign();
#define FOREIGN_END return 0; }()); }

#endif // purescript_H
