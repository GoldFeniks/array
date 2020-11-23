#pragma once
#include <memory>
#include <cstddef>
#include <utility>
#include <algorithm>
#include "assert.hpp"

namespace feniks::_impl {

    template<typename T>
    concept sharable_data_type =
        std::is_default_constructible_v<T> &&
        !std::is_const_v<T> &&
        !std::is_reference_v<T> &&
        !std::is_pointer_v<T>;

    template<sharable_data_type, typename>
    struct shared_data;

    template<typename T, typename ST>
    class strides {

    public:

        template<typename F, typename... S>
        static shared_data<T, ST> make(const F&, const S&... sizes) {
            return _make<S...>(sizes..., std::make_index_sequence<sizeof...(S)>{});
        }

        template<size_t N>
        static shared_data<T, ST> make(const shared_data<T, ST>& sizes) {
            return _make(sizes, std::make_index_sequence<N - 1>{});
        }

    private:

        template<typename... S, size_t... I>
        static shared_data<T, ST> _make(const S&... sizes, std::index_sequence<I...>) {
            constexpr auto D = sizeof...(S) + 1;

            auto result = new std::remove_const_t<T>[D];
            result[D - 1] = 1;

            auto values = std::make_tuple(sizes..., T(1));
            ((result[D - I - 2] = result[D - I - 1] * std::get<D - I - 2>(values)), ...);
            return shared_data<T, ST>(result);
        }

        template<size_t... I>
        static shared_data<T, ST> _make(const shared_data<T, ST>& sizes, std::index_sequence<I...>) {
            constexpr auto D = sizeof...(I) + 1;

            auto result = new std::remove_const_t<T>[D + 1];
            result[D - 1] = 1;
            ((result[D - I - 2] = result[D - I - 1] * sizes[D - I - 2]), ...);
            return shared_data<T, ST>(result);
        }

    };

    template<sharable_data_type T, typename DT = size_t>
    class shared_data {

    public:

        using difference_type = DT;

        shared_data() : _data(nullptr) {}
        explicit shared_data(T* data) : _data(data) {}
        explicit shared_data(std::shared_ptr<T[]> data) : _data(data) {}
        explicit shared_data(const size_t& size) : shared_data(new T[size]) {}

        shared_data offset(const difference_type& offset) const {
            return shared_data(std::shared_ptr<T[]>(_data, _data.get() + offset));
        }

        T& operator[](const difference_type& n) const {
            return _data.get()[n];
        }

        T* data() const {
            return _data.get();
        }

        std::strong_ordering operator<=>(const shared_data& other) const {
            return _data <=> other._data;
        };

    private:

        template<sharable_data_type, typename>
        friend class shared_data;

        std::shared_ptr<T[]> _data;

    };

    template<typename T, size_t D, typename ST = size_t, typename DT = std::ptrdiff_t>
    class shared_sized_data {

    private:

        using data_type = shared_data<T, DT>;
        using sizes_type = shared_data<ST, DT>;

    public:

        using size_type = ST;
        using difference_type = DT;

        shared_sized_data() = default;

        template<typename... S, typename = std::enable_if_t<(std::is_convertible_v<S, size_type> && ...) && sizeof...(S) == D>>
        shared_sized_data(const S&... sizes) {
            allocate(sizes...);
        }

        template<typename... S, typename = std::enable_if_t<(std::is_convertible_v<S, size_type> && ...) && sizeof...(S) == D>>
        void allocate(const S&... sizes) {
            _data = data_type((sizes * ...));
            _sizes = sizes_type(new ST[sizeof...(S)] { ST(sizes)... });
            _strides = _impl::strides<ST, DT>::make(sizes...);
        }

        void allocate(const sizes_type& sizes) {
            _sizes = sizes;
            _strides = _impl::strides<ST, DT>::template make<D>(sizes);
            _data = data_type(full_size());
        }

        void allocate(const size_type& n, const sizes_type& sizes, const sizes_type& strides) {
            _sizes = sizes_type(D);
            _strides = sizes_type(D);
            std::copy(sizes.data(), sizes.data() + D - 1, _sizes.data() + 1);
            std::copy(strides.data(), strides.data() + D - 1, _strides.data() + 1);

            _sizes[0] = n;
            if constexpr (D > 1)
                _strides[0] = _strides[1] * _sizes[1];
            else
                _strides[0] = 1;

            _data = data_type(full_size());
        }

        const sizes_type& sizes() const {
            return _sizes;
        }

        const sizes_type& strides() const {
            return _strides;
        }

        ST size() const {
            return _sizes[0];
        }

        ST size(const ST& n) const {
            return _sizes[n];
        }

        ST full_size() const {
            return _full_size(std::make_index_sequence<D>{});
        }

        shared_sized_data& operator++() {
            _data = _data.offset(_strides[0]);
            return *this;
        }

        shared_sized_data operator++(int) {
            auto result = shared_sized_data(_data, _sizes, _strides);
            ++*this;
            return result;
        }

        shared_sized_data& operator--() {
            _data = _data.offset(-_strides[0]);
            return *this;
        }

        shared_sized_data operator--(int) {
            auto result = shared_sized_data(_data, _sizes, _strides);
            --*this;
            return result;
        }

        shared_sized_data& operator+=(const DT& n) {
            _data = _data.offset(n * _strides[0]);
            return *this;
        }

        shared_sized_data operator+(const DT& n) const {
            return shared_sized_data(_data.offset(n * _strides[0]), _sizes, _strides);
        }

        shared_sized_data& operator-=(const DT& n) {
            _data = _data.offset(-n * _strides[0]);
            return *this;
        }

        shared_sized_data operator-(const DT& n) const {
            return shared_sized_data(_data.offset(-n * _strides[0]), _sizes, _strides);
        }

        std::conditional_t<D == 1, T&, shared_sized_data<T, D - 1, ST>> operator[](const ST& n) const {
            if constexpr (D == 1)
                return _data[n * _strides[0]];
            else
                return shared_sized_data<T, D - 1, ST>(_data.offset(n * _strides[0]), _sizes.offset(1), _strides.offset(1));
        }

        template<size_t N, typename = std::enable_if_t<N <= D>>
        std::conditional_t<D - N == 0, T&, shared_sized_data<T, D - N, ST>> operator[](const size_type(&index)[N]) const {
            if constexpr (D - N == 0)
                return _data[_offset(index)];
            else
                return shared_sized_data<T, D - N, ST>(_data.offset(_offset(index)), _sizes.offset(N), _strides.offset(N));
        }

        shared_sized_data<T, 1, ST, DT> flatten() const {
            return shared_sized_data<T, 1, ST>(_data, sizes_type(new ST[1] { full_size() }), sizes_type(new ST[1] { 1 }));
        }

        template<typename...S, typename = std::enable_if_t<(std::is_convertible_v<S, difference_type> && ...)>>
        shared_sized_data<T, sizeof...(S), ST, DT> reshape(S... sizes) const {
            auto size = ((sizes > 0 ? sizes : 1) * ...);
            size_t count = 0;
            ((sizes > 0 ? 0 : (++count, size *= (sizes = full_size() / size))), ...);

            dynamic_assert(count <= 1, "More than 1 size is less than 0");
            dynamic_assert(full_size() == size, "New shape doesn't match total array size");

            return shared_sized_data<T, sizeof...(S), ST, DT>(_data,
                sizes_type(new ST[sizeof...(S)] { size_type(sizes)... }), _impl::strides<ST, DT>::make(sizes...));
        }

        T* data_begin() const {
            return _data.data();
        }

        T* data_end() const {
            return _data.data() + full_size();
        }

        std::strong_ordering operator<=>(const shared_sized_data& other) const {
            return _data <=> other._data;
        };

        shared_sized_data copy() const {
            data_type data(full_size());
            sizes_type sizes(D), strides(D);

            std::copy(_data.data(), _data.data() + full_size(), data.data());
            _copy(_sizes.data(), sizes.data());
            _copy(_strides.data(), strides.data());

            return shared_sized_data(std::move(data), std::move(sizes), std::move(strides));
        }

        template<size_t N>
        shared_sized_data<T, N, ST, DT> as_strided(const sizes_type& sizes, const sizes_type& strides, const size_type& offset) const {
            return shared_sized_data<T, N, ST, DT>(_data.offset(offset), sizes, strides);
        }

    private:

        template<typename, size_t, typename, typename>
        friend class shared_sized_data;

        data_type _data;
        sizes_type _sizes, _strides;

        shared_sized_data(data_type data, sizes_type sizes, sizes_type strides) :
                _data(std::move(data)), _sizes(std::move(sizes)), _strides(std::move(strides)) {}

        template<size_t... I>
        size_type _full_size(std::index_sequence<I...>) const {
            return (_sizes[I] * ...);
        }

        template<size_t N>
        size_type _offset(const size_type(&index)[N]) const {
            return _offset(index, std::make_index_sequence<N>{});
        }

        template<size_t N, size_t... I>
        size_type _offset(const size_type(&index)[N], std::index_sequence<I...>) const {
            return ((_strides[I] * index[I]) + ...);
        }

        template<typename V>
        static void _copy(const V* from, V* to) {
            _copy(from, to, std::make_index_sequence<D>{});
        }

        template<typename V, size_t... I>
        static void _copy(const V* from, V* to, std::index_sequence<I...>) {
            ((to[I] = from[I]), ...);
        }

    };

}// namespace feniks::_impl
