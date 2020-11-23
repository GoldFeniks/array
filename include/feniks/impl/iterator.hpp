#pragma once
#include <tuple>
#include <memory>
#include <cassert>
#include <cstddef>
#include <cstring>
#include <iterator>
#include <type_traits>
#include "shared_data.hpp"

namespace feniks::_impl {

    template<size_t N, typename D, typename V, typename S, typename I>
    class iterator_base {

    public:

        using pointer           = D*;
        using reference         = std::conditional_t<N == 1, D&, V>;
        using data_type         = D;
        using size_type         = S;
        using value_type        = V;
        using difference_type   = I;
        using iterator_category = std::conditional_t<N == 1, std::contiguous_iterator_tag, std::random_access_iterator_tag>;

    private:

        using shared_data_type = _impl::shared_sized_data<std::remove_const_t<data_type>, N, size_type, difference_type>;

    public:

        iterator_base() = default;
        iterator_base(const iterator_base& other) = default;
        iterator_base(iterator_base&& other) noexcept = default;
        ~iterator_base() = default;

        iterator_base(shared_data_type data) : _data(std::move(data)) {}

        iterator_base& operator=(iterator_base&& other) = default;
        iterator_base& operator=(const iterator_base& other) = default;

        std::strong_ordering operator<=>(const iterator_base& other) const {
            return _data <=> other._data;
        }

        bool operator==(const iterator_base& other) const {
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

        iterator_base& operator++() {
            ++_data;
            return *this;
        }

        iterator_base operator++(int) {
            return _data++;
        }

        iterator_base& operator--() {
            --_data;
            return *this;
        }

        iterator_base operator--(int) {
            return _data--;
        }

        iterator_base& operator+=(const difference_type& n) {
            _data += n;
            return *this;
        }

        iterator_base operator+(const difference_type& n) const {
            iterator_base result(_data);
            result += n;
            return result;
        }

        friend iterator_base operator+(const difference_type& n, const iterator_base& other) {
            return other + n;
        }

        iterator_base& operator-=(const difference_type& n) {
            _data -= n;
            return *this;
        }

        iterator_base operator-(const difference_type& n) const {
            iterator_base result(_data);
            result -= n;
            return result;
        }

        difference_type operator-(const iterator_base& other) const {
            return _data - other._data;
        }

        std::conditional_t<N == 1, reference, value_type> operator[](const difference_type& index) const {
            return _data[index];
        }

    private:

        shared_data_type _data;

    };

    template<typename Base>
    using iterator = iterator_base<Base::dimensions, typename Base::data_type,
            typename Base::value_type, typename Base::size_type,
            typename Base::difference_type>;

    template<typename Base>
    using const_iterator = iterator_base<Base::dimensions, typename Base::data_type,
            const typename Base::value_type, typename Base::size_type,
            typename Base::difference_type>;

}// namespace feniks::_impl
