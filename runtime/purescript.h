#ifndef purescript_H
#define purescript_H

#include <memory>
#include <functional>
#include <deque>
#include <map>
#include <string>

#include <iostream>

namespace purescript {

    using std::string;

    class boxed : public std::shared_ptr<void> {
        
        using fn = std::function<boxed(const boxed&)>;
        using eff_fn = std::function<boxed(void)>;

    public:
        using std::shared_ptr<void>::shared_ptr;

        boxed(const int64_t n) : std::shared_ptr<void>(std::make_shared<int64_t>(n)) {
        }

        boxed(const int n) : std::shared_ptr<void>(std::make_shared<int64_t>(n)) {
        }

        boxed(const unsigned int n) : std::shared_ptr<void>(std::make_shared<int64_t>(n)) {
        }

        boxed(const double n) : std::shared_ptr<void>(std::make_shared<double>(n)) {
        }

        boxed(const char s[]) : std::shared_ptr<void>(std::make_shared<string>(s)) {
        }

        boxed(string&& s) : std::shared_ptr<void>(std::make_shared<string>(std::move(s))) {
        }

        boxed(const std::nullptr_t) : std::shared_ptr<void>() {
        }
        
        boxed(std::initializer_list<boxed> l) : std::shared_ptr<void>(std::make_shared<std::deque<boxed>>(l)) {
        }

        boxed(std::initializer_list<std::pair<const string, boxed>> m) : std::shared_ptr<void>(std::make_shared<std::map<string, boxed>>(m)) {
        }

        template <typename T,
                  typename = typename std::enable_if<!std::is_same<boxed,T>::value>::type>
        boxed(const T& f,
              typename std::enable_if<std::is_assignable<std::function<boxed(boxed)>,T>::value>::type* = 0) : std::shared_ptr<void>(std::make_shared<fn>(f)) {
        }

        template <typename T,
                  typename = typename std::enable_if<!std::is_same<boxed,T>::value>::type>
        boxed(const T& f,
              typename std::enable_if<std::is_assignable<std::function<boxed(void)>,T>::value>::type* = 0) : std::shared_ptr<void>(std::make_shared<eff_fn>(f)) {
        }

        template <typename T,
        typename = typename std::enable_if<!std::is_same<boxed,T>::value &&
                                            std::is_convertible<T,fn>::value>::type>
        auto operator=(const T& right) -> boxed& {
            if (std::shared_ptr<void>::operator bool()) {
                auto& f = *static_cast<fn*>(get());
                f = right;
            } else {
                reset(new fn(right));
            }
            return *this;
        }

        auto operator()(const boxed& arg) const -> boxed {
            auto& f = *static_cast<fn*>(get());
            return f(arg);
        }

        auto operator()() const -> boxed {
            auto& f = *static_cast<eff_fn*>(get());
            return f();
        }

        auto operator[](const char key[]) const -> boxed {
          const auto& dict = *static_cast<const std::map<const string, boxed>*>(get());
          return dict.at(key);
        }

    };

    template <typename T, typename... Args>
    inline auto box(Args&&... args) -> boxed {
        return std::make_shared<T>(std::forward<Args>(args)...);
    }

    template <typename T>
    inline auto unbox(const boxed& b) -> const T& {
        const auto& unboxed = *static_cast<const T*>(b.get());
        return unboxed;
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

    using fn_t = std::function<boxed(const boxed&)>;
    using dict_t = std::map<const string, boxed>;
    using array_t = std::deque<boxed>;

    using as_array = std::initializer_list<boxed>;
    using as_dict = std::initializer_list<std::pair<const string, boxed>>;

    constexpr auto undefined = nullptr;

} // namespace purescript

#define FOREIGN_EXPORTS(NS) namespace NS { static const auto& NS ## ＿foreign_init = []() { dict_t& foreign = ＿foreign＿();
#define END_FOREIGN_EXPORTS return foreign; }(); }

#endif // purescript_H
