#pragma once
#include <type_traits>

namespace feniks {

    template<typename To, typename From, typename... Rest>
    struct is_convertible_all {

        constexpr static bool value = std::is_convertible_v<From, To> && is_convertible_all<To, Rest...>::value;

        using value_type = bool;
        using type = std::integral_constant<bool, value>;

        explicit operator value_type() { return value; }
        value_type operator()() { return value; }

    };

    template<typename To, typename From>
    struct is_convertible_all<To, From> {

        constexpr static bool value = std::is_convertible_v<From, To>;

        using value_type = bool;
        using type = std::integral_constant<bool, value>;

        explicit operator value_type() { return value; }
        value_type operator()() { return value; }

    };

    template<typename To, typename From, typename... Rest>
    constexpr bool is_convertible_all_v = is_convertible_all<To, From, Rest...>::value;

}