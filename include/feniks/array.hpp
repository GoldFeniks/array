#pragma once
#include <span>
#include <tuple>
#include <memory>
#include <cassert>
#include <cstddef>
#include <cstring>
#include <type_traits>
#include "iterator.hpp"
#include "type_traits.hpp"
#include "shared_data.hpp"

namespace feniks {

    namespace _impl {

        template<typename, size_t>
        class array;

        template<typename T, size_t N>
        struct array_type {

            using type = array<std::remove_reference_t<T>, N>;

        };

        template<typename T>
        struct array_type<T, 0> {

            using type = T;

        };

        template<typename T, size_t N>
        using array_type_t = typename array_type<T, N>::type;

        template<typename T, size_t D = 1>
        class array {

        public:

            static constexpr auto dimensions = D;

            using data_type              = T;
            using size_type              = size_t;
            using value_type             = array_type_t<T, D - 1>;
            using difference_type        = std::ptrdiff_t;

            using pointer                = data_type*;
            using const_pointer          = const data_type*;

            using reference              = std::conditional_t<D == 1, data_type&, value_type>;
            using const_reference        = std::conditional_t<D == 1, const data_type&, array<const data_type, D - 1>>;

            using data_reference         = data_type&;
            using const_data_reference   = const data_type&;

            using iterator               = feniks::iterator<array>;
            using const_iterator         = feniks::const_iterator<array<const T, D>>;

            using reverse_iterator       = std::reverse_iterator<iterator>;
            using const_reverse_iterator = std::reverse_iterator<const_iterator>;

            array() = delete;

            array(const array& other) : _data(other._data) {}
            array(array&& other) : _data(std::move(other._data)) {}

            array& operator=(const array& other) & {
                _data = other._data;
                return *this;
            }

            array& operator=(const array& other) && {
                this->assign(other);
                return *this;
            }

            array& operator=(array&& other) & {
                _data = std::move(other._data);
                return *this;
            }

            array& operator=(array&& other) && {
                return std::move(*this) = other;
            }

            template<typename... S, typename = std::enable_if_t<(std::is_convertible_v<S, size_type> && ...) && (sizeof...(S) == D)>>
            explicit array(const S&... sizes) : _data(sizes...) {}

            array(const std::initializer_list<array_type_t<data_type, D - 1>>& data) {
                if constexpr (D == 1) {
                    _data.allocate(data.size());
                    std::copy(data.begin(), data.end(), _data.data_begin());
                } else {
                    auto it = data.begin();
                    _data.allocate(data.size(), it->_data.sizes(), it->_data.strides());
                    auto d = _data.data_begin();

                    for (; it != data.end(); ++it, d += _data.strides()[0]) {
                        dynamic_assert(_all_equal<D - 1>(_data.sizes().data() + 1, it->_data.sizes().data()), "Array sizes differ");
                        std::copy(it->_data.data_begin(), it->_data.data_end(), d);
                    }
                }
            }

            template<typename V, typename = std::enable_if_t<std::is_same_v<std::remove_const_t<V>, data_type>>>
            void assign(const array<V, D>& other) {
                dynamic_assert(_all_equal(_data.sizes().data(), other._data.sizes().data()), "Incorrect assigned array size");
                std::copy(other._data.data_begin(), other._data.data_end(), _data.data_begin());
            }

            template<typename It, typename = std::enable_if_t<!std::is_const_v<data_type>, It>>
            void assign(It first, It last) {
                using iter_data_t = std::remove_reference_t<decltype(*first)>;

                if constexpr (std::is_same_v<std::remove_const_t<iter_data_t>, data_type>) {
                    dynamic_assert(std::distance(first, last) == full_size(), "Incorrect assigned data size");
                    std::copy(first, last, _data.data_begin());
                } else if constexpr (std::is_same_v<array<data_type, D - 1>, iter_data_t> ||
                                     std::is_same_v<array<const data_type, D - 1>, iter_data_t>) {
                    dynamic_assert(std::distance(first, last) == size(), "Incorrect assigned data size");

                    size_type index = 0;
                    while (first != last) {
                        std::copy(first->_data.data_begin(), first->_data.data_end(), _data[index].data());
                        ++first;
                    }
                } else
                    static_assert(!std::is_same_v<It, It>, "Can't assign to data");
            }

            template<typename V, typename = std::enable_if_t<!std::is_const_v<data_type>, V>>
            void fill(const V& value) {
                if constexpr (std::is_same_v<std::remove_const_t<V>, data_type>)
                    std::fill(_data.data_begin(), _data.data_end(), value);
                else if constexpr (std::is_same_v<array<data_type, D - 1>, V> ||
                                   std::is_same_v<array<const data_type, D - 1>, V>) {
                    for (size_type i = 0; i < size(); ++i)
                        std::copy(value._data.data_begin(), value._data.data_end(), _data[i].data());
                } else
                    static_assert(!std::is_same_v<V, V>, "Can't fill data");
            }

            array* operator->() {
                return this;
            }

            const array* operator->() const {
                return *this;
            }

            size_type full_size() const {
                return _data.full_size();
            }

            size_type size() const {
                return _data.size();
            }

            size_type size(const size_type& n) const {
                return _data.size(n);
            }

            array_type_t<data_reference, D - 1> operator[](const size_type& index) {
                return _data[index];
            }

            array_type_t<const_data_reference, D - 1> operator[](const size_type& index) const {
                return _data[index];
            }

            array_type_t<data_reference, D - 1> at(const size_type& index) {
                dynamic_assert<std::out_of_range>(index >= 0 && index < size(), "Incorrect index value");
                return (*this)[index];
            }

            array_type_t<const_data_reference, D - 1> at(const size_type& index) const {
                dynamic_assert<std::out_of_range>(index >= 0 && index < size(), "Incorrect index value");
                return (*this)[index];
            }

            template<size_t N, typename = std::enable_if_t<N <= D>>
            array_type_t<data_reference , D - N> operator[](const size_type(&index)[N]) {
                if constexpr (D == N)
                    return _data[index];
                else
                    return array_type_t<T&, D - N>(_data[index]);
            }

            template<size_t N, typename = std::enable_if_t<N <= D>>
            array_type_t<const_data_reference , D - N> operator[](const size_type(&index)[N]) const {
                if constexpr (D == N)
                    return _data[index];
                else
                    return array_type_t<const T&, D - N>(_data[index]);
            }

            template<size_t N, typename = std::enable_if_t<N <= D>>
            array_type_t<data_reference, D - N> at(const size_type(&index)[N]) {
                dynamic_assert<std::out_of_range>(_check_index(index), "Incorrect index value");
                return (*this)[index];
            }

            template<typename... S, typename = std::enable_if_t<(sizeof...(S) <= D) && (std::is_convertible_v<S, size_type> && ...)>>
            array_type_t<data_reference, D - sizeof...(S)> at(const S&... index) {
                return this->at({ size_type(index) ... });
            }

            template<size_t N, typename = std::enable_if_t<N <= D>>
            array_type_t<const_data_reference, D - N> at(const size_type(&index)[N]) const {
                dynamic_assert<std::out_of_range>(_check_index(index), "Incorrect index value");
                return (*this)[index];
            }

            template<typename... S, typename = std::enable_if_t<(sizeof...(S) <= D) && (std::is_convertible_v<S, size_type> && ...)>>
            array_type_t<data_reference, D - sizeof...(S)> at(const S&... index) const {
                return this->at({ size_type(index) ... });
            }

            array<data_type, 1> flatten() {
                return array<data_type, 1>(_data.flatten());
            }

            array<const data_type, 1> flatten() const {
                return array<const data_type, 1>(_data.flatten());
            }

            template<typename...S, typename = std::enable_if_t<(std::is_convertible_v<S, size_type> && ...) && (sizeof...(S) > 0)>>
            array<data_type, sizeof...(S)> reshape(const S&... sizes) {
                return _data.reshape(sizes...);
            }

            template<typename...S, typename = std::enable_if_t<(std::is_convertible_v<S, size_type> && ...) && (sizeof...(S) > 0)>>
            array<const data_type, sizeof...(S)> reshape(const S&... sizes) const {
                return _data.reshape(sizes...);
            }

            array<std::remove_const_t<data_type>, D> copy() const {
                return _data.copy();
            }

            iterator begin() noexcept {
                return iterator(_data);
            }

            iterator end() noexcept {
                return iterator(_data + size());
            }

            const_iterator begin() const noexcept {
                return const_iterator(_data);
            }

            const_iterator end() const noexcept {
                return const_iterator(_data + size());
            }

            reverse_iterator rbegin() noexcept {
                return std::make_reverse_iterator(end());
            }

            reverse_iterator rend() noexcept {
                return std::make_reverse_iterator(begin());
            }

            const_reverse_iterator rbegin() const noexcept {
                return std::make_reverse_iterator(end());
            }

            const_reverse_iterator rend() const noexcept {
                return std::make_reverse_iterator(begin());
            }

            pointer data() {
                return _data.data_begin();
            }

            const_pointer data() const {
                return _data.data_begin();
            }

            array<const data_type, D> as_const() const {
                return _data;
            }

        protected:

            using shared_data_t = shared_sized_data<std::remove_const_t<T>, D, size_type, difference_type>;

            shared_data_t _data;

            template<size_t, typename, typename, typename, typename>
            friend class _impl::iterator;

            template<typename, size_t>
            friend class array;

            array(shared_data_t data) : _data(std::move(data)) {}

            template<size_t N>
            bool _check_index(const size_type(&index)[N]) {
                return _check_index(index, std::make_index_sequence<N>{});
            }

            template<size_t N, size_t... I>
            bool _check_index(const size_type(&index)[N], std::index_sequence<I...>) {
                return ((index[I] >= 0 && index[I] < _data.sizes()[I]) &&  ...);
            }

            template<size_t N = D>
            bool _all_equal(const size_type* a, const size_type* b) {
                return _all_equal(a, b, std::make_index_sequence<N>{});
            }

            template<size_t... I>
            bool _all_equal(const size_type* a, const size_type* b, std::index_sequence<I...>) {
                return ((a[I] == b[I]) && ...);
            }

        };

    }// namespace _impl

    template<typename T, size_t N = 1>
    using array = _impl::array_type_t<T, N>;

}// namespace feniks
