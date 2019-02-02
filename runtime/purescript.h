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
#include <limits>
#include "string_literal_dict.h"


namespace purescript {

    using std::string;

    class boxed {
        std::shared_ptr<void> shared;

    public:
        union {
            int _int_;
            double _double_;
            bool _bool_;
        };

        class fn_t {
        public:
            virtual ~fn_t() {}
            virtual auto operator ()(const boxed&) const -> boxed = 0;
        };

        template <typename T>
        class fn_template_t : public fn_t {
            T fn;
        public:
            fn_template_t(T f) noexcept : fn(std::move(f)) {}
            auto operator ()(const boxed& arg) const -> boxed override {
                return fn(arg);
            }
        };

        class eff_fn_t {
        public:
            virtual ~eff_fn_t() {}
            virtual auto operator ()() const -> boxed = 0;
        };

        template <typename T>
        class eff_fn_template_t : public eff_fn_t {
            T fn;
        public:
            eff_fn_template_t(T f) noexcept : fn(std::move(f)) {}
            auto operator ()() const -> boxed override {
                return fn();
            }
        };

        using dict_t = string_literal_dict_t<boxed>;
        using array_t = std::vector<boxed>;

    public:
        boxed() noexcept : shared() {}
        boxed(const std::nullptr_t) noexcept : shared() {}

        template <typename T>
        boxed(std::shared_ptr<T>&& other) noexcept : shared(std::move(other)) {}

        template <typename T>
        boxed(const std::shared_ptr<T>& other) : shared(other) {}

        boxed(const int n) noexcept : _int_(n) {}

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
              : shared(std::make_shared<fn_template_t<T>>(std::move(f))) {
        }

        template <typename T,
                  typename = typename std::enable_if<!std::is_same<boxed,T>::value>::type>
        boxed(T f,
              typename std::enable_if<std::is_same<decltype(std::declval<T>()()),
                                                   boxed>::value>::type* = 0)
              : shared(std::make_shared<eff_fn_template_t<T>>(std::move(f))) {
        }

        inline auto get() const noexcept -> void * {
            return shared.get();
        }

        auto operator()(const boxed& arg) const -> boxed {
            auto& f = *static_cast<fn_t*>(shared.get());
            return f(arg);
        }

        auto operator()() const -> boxed {
            auto& f = *static_cast<eff_fn_t*>(shared.get());
            return f();
        }

        auto operator[](const char key[]) const -> const boxed& {
          return (*static_cast<const dict_t*>(shared.get()))[key];
        }

        auto operator[](const char key[]) -> boxed& {
          return (*static_cast<dict_t*>(shared.get()))[key];
        }

#if !defined(NDEBUG) // if debug build
        auto operator[](const int index) const -> const boxed& {
            return static_cast<const array_t*>(shared.get())->at(index);
        }

        auto operator[](const int index) -> boxed& {
            return static_cast<array_t*>(shared.get())->at(index);
        }
#else  // not debug build
        auto operator[](const int index) const -> const boxed& {
            return (*static_cast<const array_t*>(shared.get()))[index];
        }

        auto operator[](const int index) -> boxed& {
            return (*static_cast<array_t*>(shared.get()))[index];
        }
#endif // !defined(NDEBUG)

        class weak {
            std::weak_ptr<void> wptr;

            public:
            weak() = delete;
            weak(boxed& b) : wptr(b.shared) {}

            operator boxed() const {
#if defined(NDEBUG)
                return wptr.lock();
#else
                return static_cast<std::shared_ptr<void>>(wptr);
#endif
            }

        }; // class boxed::weak

        class recur {
            std::shared_ptr<boxed> sptr;
            std::shared_ptr<boxed::weak> wptr;

            public:
            recur() : sptr(std::make_shared<boxed>())
                  , wptr(std::make_shared<boxed::weak>(*sptr)) {}

            operator const boxed&() const {
                return *sptr;
            }

            operator boxed&() {
                return *sptr;
            }

            auto operator()() const -> boxed {
                return (*sptr)();
            }

            auto operator()(const boxed& arg) const -> boxed {
                return (*sptr)(arg);
            }

            template <typename T>
            auto operator=(T&& right) -> recur& {
                *sptr = std::forward<T>(right);
                *wptr = *sptr;
                return *this;
            }

            class weak {
                std::shared_ptr<boxed::weak> wptr;
            public:
                weak(const recur& r) : wptr(r.wptr) {}

                operator boxed() const {
                    return *wptr;
                }

                auto operator()() const -> boxed {
                    return static_cast<boxed>(*wptr)();
                }

                auto operator()(const boxed& arg) const -> boxed {
                    return static_cast<boxed>(*wptr)(arg);
                }
            }; // class recur::weak
        }; // class boxed::recur

    }; // class boxed

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
