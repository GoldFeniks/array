#pragma once
#include <tuple>
#include <memory>
#include <cassert>
#include <cstddef>
#include <cstring>
#include <iterator>
#include <type_traits>
#include "type_traits.hpp"

namespace feniks {

    namespace _impl {

        template<size_t N, typename D, typename V, typename S, typename I>
        struct data_wrapper {

            D* data    = nullptr;
            const S* sizes   = nullptr;
            const S* strides = nullptr;

            data_wrapper() = default;

            data_wrapper(D* data, const S* sizes, const S* strides)
                    : data(data), sizes(sizes), strides(strides) {}

            V operator*() const {
                return V(data, sizes + 1, strides + 1);
            }

            V operator->() const {
                return V(data, sizes + 1, strides + 1);
            }

            V operator[](const I& index) const {
                return V(data + index * *strides, sizes + 1, strides + 1);
            }

            data_wrapper& operator++() {
                data += *strides;
                return *this;
            }

            data_wrapper& operator+=(const I& n) {
                data += n * *strides;
                return *this;
            }

            data_wrapper& operator--() {
                data -= *strides;
                return *this;
            }

            data_wrapper& operator-=(const I& n) {
                return *this += -n;
            }

            std::strong_ordering operator<=>(const data_wrapper& other) const {
                return data <=> other.data;
            }

            I operator-(const data_wrapper& other) const {
                return (data - other.data) / *strides;
            }

        };

        template<typename D, typename V, typename S, typename I>
        struct data_wrapper<1, D, V, S, I> {

            D* data = nullptr;

            data_wrapper() = default;

            data_wrapper(D* data, const S*, const S*) : data(data) {}

            D& operator*() const {
                return *data;
            }

            D* operator->() const {
                return data;
            }

            D& operator[](const I& index) const {
                return *(data + index);
            }

            data_wrapper& operator++() {
                ++data;
                return *this;
            }

            data_wrapper& operator+=(const I& n) {
                data += n;
                return *this;
            }

            data_wrapper& operator--() {
                --data;
                return *this;
            }

            data_wrapper& operator-=(const I& n) {
                return *this += -n;
            }

            std::strong_ordering operator<=>(const data_wrapper& other) const {
                return data <=> other.data;
            }

            I operator-(const data_wrapper& other) const {
                return data - other.data;
            }

        };

        template<size_t N>
        struct iterator_helper {

            template<typename D, typename V, typename S, typename I>
            class iterator {

            public:

                using pointer           = D*;
                using reference         = V&;
                using data_type         = D;
                using size_type         = S;
                using value_type        = V;
                using difference_type   = I;
                using iterator_category = std::conditional_t<N == 1, std::contiguous_iterator_tag, std::random_access_iterator_tag>;

                iterator() = default;
                iterator(const iterator& other) = default;
                iterator(iterator&& other) noexcept = default;
                ~iterator() = default;

                iterator(pointer data, size_type* sizes, size_type* strides) : _data(data, sizes, strides) {}

                iterator& operator=(iterator&& other) {
                    _data = std::move(other._data);
                    return *this;
                }

                iterator& operator=(const iterator& other) {
                    _data = other._data;
                    return *this;
                }

                std::strong_ordering operator<=>(const iterator& other) const {
                    return _data <=> other._data;
                }

                bool operator==(const iterator& other) const {
                    return (*this) <=> other == 0;
                }

                std::conditional_t<N == 1, reference, value_type> operator*() const {
                    return *_data;
                }

                auto operator->() const {
                    return _data.operator->();
                }

                iterator& operator++() {
                    ++_data;
                    return *this;
                }

                iterator operator++(int) {
                    return ++iterator(_data);
                }

                iterator& operator--() {
                    --_data;
                    return *this;
                }

                iterator operator--(int) {
                    return --iterator(_data);
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

                std::conditional_t<N == 1, reference, value_type>  operator[](const difference_type& index) const {
                    return _data[index];
                }

            private:

                data_wrapper<N, data_type, value_type, size_type, difference_type> _data;

                iterator(data_wrapper<N, data_type, value_type, size_type, difference_type> data) : _data(std::move(data)) {}

            };

        };

    }// namespace _impl

    template<typename Base>
    using iterator = typename _impl::iterator_helper<Base::dimensions>::template iterator<typename Base::data_type,
            typename Base::value_type, typename Base::size_type, typename Base::difference_type>;

    template<typename Base>
    using const_iterator = typename _impl::iterator_helper<Base::dimensions>::template iterator<const typename Base::data_type,
            const typename Base::value_type, typename Base::size_type, typename Base::difference_type>;

}