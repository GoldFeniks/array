#pragma once
#include <span>
#include <tuple>
#include <memory>
#include <vector>
#include <cassert>
#include <cstddef>
#include <cstring>
#include <type_traits>
#include "impl/types.hpp"
#include "impl/iterator.hpp"
#include "impl/shared_data.hpp"

namespace feniks {

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

        using iterator               = feniks::_impl::iterator<array>;
        using const_iterator         = feniks::_impl::const_iterator<array<const T, D>>;

        using reverse_iterator       = std::reverse_iterator<iterator>;
        using const_reverse_iterator = std::reverse_iterator<const_iterator>;

    private:

        using sizes_t = _impl::shared_data<size_type, difference_type>;
        using shared_data_t = _impl::shared_sized_data<std::remove_const_t<T>, D, size_type, difference_type>;

    public:

        array() {
            _allocate_zero(std::make_integer_sequence<size_type, D>{});
        };

        array(const array& other) : _data(other._data) {}
        array(array&& other) noexcept : _data(std::move(other._data)) {}

        array& operator=(const array& other) & {
            _data = other._data;
            return *this;
        }

        array& operator=(const array& other) && {
            this->assign(other);
            return *this;
        }

        array& operator=(array&& other) & noexcept {
            _data = std::move(other._data);
            return *this;
        }

        array& operator=(array&& other) && noexcept {
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
                    _impl::dynamic_assert(_all_equal<D - 1>(_data.sizes().data() + 1, it->_data.sizes().data()), "Array sizes differ");
                    std::copy(it->_data.data_begin(), it->_data.data_end(), d);
                }
            }
        }

        explicit array(const _impl::ndvector_t<data_type, D>& data) {
            const auto sizes = _get_vector_sizes<data_type, D>(data);
            _data.allocate(sizes);
            _copy_vector_values<data_type, D>(data, _data.data_begin());
        }

        template<typename V, typename = std::enable_if_t<std::is_same_v<std::remove_const_t<V>, data_type>>>
        void assign(const array<V, D>& other) {
            _impl::dynamic_assert(_all_equal(_data.sizes().data(), other._data.sizes().data()), "Incorrect assigned array size");
            std::copy(other._data.data_begin(), other._data.data_end(), _data.data_begin());
        }

        template<typename It, typename = std::enable_if_t<!std::is_const_v<data_type>, It>>
        void assign(It first, It last) {
            using iter_data_t = std::remove_reference_t<decltype(*first)>;

            if constexpr (std::is_same_v<std::remove_const_t<iter_data_t>, data_type>) {
                _impl::dynamic_assert(std::distance(first, last) == full_size(), "Incorrect assigned data size");
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

        [[nodiscard]] size_type full_size() const {
            return _data.full_size();
        }

        [[nodiscard]] size_type size() const {
            return _data.size();
        }

        [[nodiscard]] size_type size(const size_type& n) const {
            return _data.size(n);
        }

        array_type_t<data_reference, D - 1> front() {
            return _data[0];
        }

        array_type_t<const_data_reference, D - 1> front() const {
            return _data[0];
        }

        array_type_t<data_reference, D - 1> back() {
            return _data[size() - 1];
        }

        array_type_t<const_data_reference, D - 1> back() const {
            return _data[size() - 1];
        }

        array_type_t<data_reference, D - 1> operator[](const size_type& index) {
            return _data[index];
        }

        array_type_t<const_data_reference, D - 1> operator[](const size_type& index) const {
            return _data[index];
        }

        array_type_t<data_reference, D - 1> at(const size_type& index) {
            _impl::dynamic_assert<std::out_of_range>(index >= 0 && index < size(), "Incorrect index value");
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
            _impl::dynamic_assert<std::out_of_range>(_check_index(index), "Incorrect index value");
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

        [[nodiscard]] sizes_t strides() const {
            return _data.strides().copy(D);
        }

        [[nodiscard]] sizes_t sizes() const {
            return _data.sizes().copy(D);
        }

        template<size_t N>
        array<data_type, N> as_strided(const sizes_t& sizes, const sizes_t& strides, const size_type& offset = 0) {
            return _data.template as_strided<N>(sizes, strides, offset);
        }

        template<size_t N>
        array<data_type, N> as_strided(const size_type(&sizes)[N], const size_type(&strides)[N], const size_type& offset = 0) {
            return as_strided<N>(sizes_t(_array_to_pointer(sizes)), sizes_t(_array_to_pointer(strides)), offset);
        }

        template<size_t N>
        array<data_type, N> as_strided(const sizes_t& sizes, const size_type(&strides)[N], const size_type& offset = 0) {
            return as_strided<N>(sizes, sizes_t(_array_to_pointer(strides)), offset);
        }

        template<size_t N>
        array<data_type, N> as_strided(const size_type(&sizes)[N], const sizes_t& strides, const size_type& offset = 0) {
            return as_strided<N>(sizes_t(_array_to_pointer(sizes)), strides, offset);
        }

    private:

        shared_data_t _data;

        template<size_t, typename, typename, typename, typename>
        friend class _impl::iterator_base;

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

        template<size_t N>
        size_type* _array_to_pointer(const size_type(&values)[N]) {
            return _array_to_pointer(values, std::make_index_sequence<N>{});
        }

        template<size_t... I>
        size_type* _array_to_pointer(const size_type(&values)[sizeof...(I)], std::index_sequence<I...>) {
            return new size_type[sizeof...(I)] { values[I]... };
        }

        template<typename V, size_t N = D>
        static sizes_t _get_vector_sizes(const _impl::ndvector_t<V, N>& data) {
            sizes_t sizes(N);
            _get_vector_sizes<V, N>(data, sizes);
            _check_vector_sizes<V, N>(data, sizes);
            return sizes;
        }

        template<typename V, size_t N = D>
        static void _get_vector_sizes(const _impl::ndvector_t<V, N>& data, sizes_t& sizes) {
            _impl::dynamic_assert(data.size() > 0, "Vector cannot have 0 size");
            sizes[D - N] = data.size();
            if constexpr (N > 1)
                _get_vector_sizes<V, N - 1>(data[0], sizes);
        }

        template<typename V, size_t N = D>
        static void _check_vector_sizes(const _impl::ndvector_t<V, N>& data, sizes_t& sizes) {
            _impl::dynamic_assert(data.size() == sizes[D - N], "Subvectors must have the same size");
            if constexpr (N > 1)
                for (const auto& it : data)
                    _check_vector_sizes<V, N - 1>(it, sizes);
        }

        template<typename V, size_t N = D>
        void _copy_vector_values(const _impl::ndvector_t<V, N>& values, pointer data) {
            if constexpr (N == 1)
                std::copy(values.data(), values.data() + values.size(), data);
            else
                for (const auto& it : values) {
                    _copy_vector_values<V, N - 1>(it, data);
                    data += _data.strides()[D - N];
                }
        }

        template<size_t... I>
        void _allocate_zero(std::integer_sequence<size_type, I...>) {
            _data.template allocate((I * 0)...);
        }
    };

}// namespace feniks
