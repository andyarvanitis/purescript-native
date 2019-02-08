///////////////////////////////////////////////////////////////////////////////
//
// Module      :  recursion.h
// Copyright   :  (c) Andy Arvanitis 2019
// License     :  BSD
//
// Maintainer  :  Andy Arvanitis
// Stability   :  experimental
// Portability :
//
// Runtime types to support recursion
//
///////////////////////////////////////////////////////////////////////////////
//
#ifndef purescript_recursion_H
#define purescript_recursion_H

#include <memory>

namespace purescript {

    namespace _template_ {

    template <typename T>
    class weak {
        std::weak_ptr<void> wptr;

        public:
        weak() = delete;
        weak(T& b) : wptr(b.shared) {}

        auto shared() const -> std::shared_ptr<void> {
#if defined(NDEBUG)
            return wptr.lock();
#else
            return static_cast<std::shared_ptr<void>>(wptr);
#endif
        }

    }; // class weak

    template <typename T>
    class recur {
        std::shared_ptr<T> sptr;
        std::shared_ptr<typename T::weak> wptr;

        public:
        recur() : sptr(std::make_shared<T>())
              , wptr(std::make_shared<typename T::weak>(*sptr)) {}

        auto shared() const -> std::shared_ptr<void> {
            return sptr->shared;
        }

        auto operator()() const -> T {
            return (*sptr)();
        }

        auto operator()(const T& arg) const -> T {
            return (*sptr)(arg);
        }

        template <typename U>
        auto operator=(U&& right) -> recur& {
            *sptr = std::forward<U>(right);
            *wptr = *sptr;
            return *this;
        }

        class weak {
            std::shared_ptr<typename T::weak> wptr;
        public:
            weak(const recur& r) : wptr(r.wptr) {}

            auto shared() const -> std::shared_ptr<void> {
                return wptr->shared();
            }

            auto operator()() const -> T {
                return static_cast<T>(*wptr)();
            }

            auto operator()(const T& arg) const -> T {
                return static_cast<T>(*wptr)(arg);
            }
        }; // class recur::weak
    }; // class recur

    } // namespace _template_
} // namespace purescript

#endif // purescript_recursion_H
