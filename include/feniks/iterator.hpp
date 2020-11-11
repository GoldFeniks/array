#pragma once
#include <tuple>
#include <memory>
#include <cassert>
#include <cstddef>
#include <cstring>
#include <iterator>
#include <type_traits>
#include "type_traits.hpp"
#include "shared_data.hpp"

namespace feniks {

    namespace _impl {

        template<size_t N, typename D, typename V, typename S, typename I>
        class iterator {

        public:

            using pointer           = D*;
            using reference         = std::conditional_t<N == 1, D&, V>;
            using data_type         = D;
            using size_type         = S;
            using value_type        = V;
            using difference_type   = I;
            using iterator_category = std::conditional_t<N == 1, std::contiguous_iterator_tag, std::random_access_iterator_tag>;

        private:

            using shared_data_type = shared_sized_data<std::remove_const_t<data_type>, N, size_type, difference_type>;

        public:

            iterator() = default;
            iterator(const iterator& other) = default;
            iterator(iterator&& other) noexcept = default;
            ~iterator() = default;

            iterator(shared_data_type data) : _data(std::move(data)) {}

            iterator& operator=(iterator&& other) = default;
            iterator& operator=(const iterator& other) = default;

            std::strong_ordering operator<=>(const iterator& other) const {
                return _data <=> other._data;
            }

            bool operator==(const iterator& other) const {
                return (*this) <=> other == 0;
            }

            std::conditional_t<N == 1, reference, value_type> operator*() const {
                return _data[0];
            }

            auto operator->() const {
                if constexpr (N == 1)
                    return _data.data_begin();
                else
                    return _data[0];
            }

            iterator& operator++() {
                ++_data;
                return *this;
            }

            iterator operator++(int) {
                return _data++;
            }

            iterator& operator--() {
                --_data;
                return *this;
            }

            iterator operator--(int) {
                return _data--;
            }

            iterator& operator+=(const difference_type& n) {
                _data += n;
                return *this;
            }

            iterator operator+(const difference_type& n) const {
                iterator result(_data);
                result += n;
                return result;
            }

            friend iterator operator+(const difference_type& n, const iterator& other) {
                return other + n;
            }

            iterator& operator-=(const difference_type& n) {
                _data -= n;
                return *this;
            }

            iterator operator-(const difference_type& n) const {
                iterator result(_data);
                result -= n;
                return result;
            }

            difference_type operator-(const iterator& other) const {
                return _data - other._data;
            }

            std::conditional_t<N == 1, reference, value_type> operator[](const difference_type& index) const {
                return _data[index];
            }

        private:

            shared_data_type _data;

        };

    }// namespace _impl

    template<typename Base>
    using iterator = typename _impl::iterator<Base::dimensions, typename Base::data_type,
            typename Base::value_type, typename Base::size_type,
            typename Base::difference_type>;

    template<typename Base>
    using const_iterator = typename _impl::iterator<Base::dimensions, typename Base::data_type,
            const typename Base::value_type, typename Base::size_type,
            typename Base::difference_type>;

}