#pragma once
#include <cstddef>
#include <memory>

namespace feniks {


    template<typename T, size_t D, typename Allocator = std::allocator<T>>
    class array {

    public:

        typedef std::conditional_t<D == 1, T, array<T, D - 1, Allocator>> value_type;
        typedef Allocator allocator_type;
        typedef size_t size_type;
        typedef std::ptrdiff_t difference_type;
        typedef value_type& reference;
        typedef const value_type const_reference;

        static constexpr auto dimensions = D;

    };



}