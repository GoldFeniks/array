#pragma once
#include <vector>
#include <cstddef>

namespace feniks::_impl {

    template<typename T, size_t N>
    struct ndvector {

        using type = std::vector<typename ndvector<T, N - 1>::type>;

    };

    template<typename T>
    struct ndvector<T, 0> {

        using type = T;

    };

    template<typename T, size_t N>
    using ndvector_t = typename ndvector<T, N>::type;

}// namespace feniks::_impl
